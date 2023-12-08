#lang racket

(require racket/generic)
(require racket/lazy-require)

(require struct-plus-plus)
(require 2htdp/image)

(require Q/Common/config)
(require Q/Common/tile)
(require Q/Lib/list)
(require Q/Lib/test)
(require Q/Common/interfaces/serializable)
(require Q/Common/interfaces/playable)

(provide
 player-state++
 set-player-state-payload
 set-player-state-name
 hash->player-state++
 player-name?
 (struct-out player-state)
 (contract-out
  [make-player-state
   (->* ((listof tile?))
        (any/c)
        player-state?)]
  [add-to-hand
   (-> player-state? (listof tile?)
       player-state?)]
  [remove-from-hand
   (->i ([ps  player-state?]
         [tls (listof tile?)])
        #:pre/name (ps tls)
        "hand must contain given tiles!"
        (contains-all? (player-state-hand ps) tls)
        [result player-state?])]
  [clear-hand
   (-> player-state? player-state?)]
  [add-points
   (-> player-state?
       natural?
       player-state?)]
  [render-player-state
   (-> player-state?
       image?)]))


;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================


#; {Any -> Boolean}
;; Determines if the provided value is a PlayerName.
(define (player-name? name)
  (cond
    [(symbol? name) (regexp-match "^[a-zA-Z0-9]+$" (symbol->string name))]
    [else #f]))

#; {type PlayerState = (player-state Natural [Listof Tile] PlayerName Any)}
;; A PlayerState represents a participating player's state during any instant of time, containing
;; the points the player accrued during the game, along with the tiles in their hand during a move,
;; and a payload of type `Any`.
;; INVARIANT: The hand of a player state has a length L such that 0 ≤ L ≤ (*hand-size*).
(struct++ player-state
          ([(score 0)    natural?]
           [hand         (listof tile?)]
           [(name #f)    (or/c player-name? #f)]
           [(payload #f) any/c])
          #:transparent
          #:methods gen:serializable
          [(define/generic ->jsexpr* ->jsexpr)
           (define (->jsexpr ps)
             (match-define [player-state score hand name _payload] ps)
             (hash 'name (->jsexpr name)
                   'score score
                   'tile* (map ->jsexpr* hand)))])

#; {type PlayerName = Symbol}
;; A PlayerName is a Symbol that must consisent of 1-20 alphanumeric characters.


;; ========================================================================================
;; CORE FUNCTIONALITY
;; ========================================================================================


#; {[Listof Tile] -> PlayerState}
;; Creates a player with the given hand, a default score of 0,
;; and the given optional payload (default of #f).
(define (make-player-state hand [payload #f])
  (player-state++ #:hand hand #:payload payload))


#; {([Listof Tile] -> [Listof Tile]) PlayerState -> PlayerState}
(define (apply-hand f ps)
  (set-player-state-hand ps (f (player-state-hand ps))))


#; {PlayerState [Listof Tile] -> PlayerState}
;; Adds the given tiles to the hand of this player state.
(define (add-to-hand ps new-tiles)
  (apply-hand (curryr append new-tiles) ps))


#; {JPlayer -> PlayerState}
(define (hash->player-state++ jp)
  (define score (hash-ref jp 'score))
  (define hand (map hash->tile++ (hash-ref jp 'tile*)))
  (define name
    (if (hash-has-key? jp 'name)
        (string->symbol (hash-ref jp 'name))
        #f))
  (player-state++ #:score score
                  #:hand  hand
                  #:name  name))


#; {PlayerState [Listof Tile] -> PlayerState}
;; Removes the given tiles from the hand of the given player state.
(define (remove-from-hand state tiles)
  (apply-hand (curry remove-from tiles) state))


#; {PlayerState -> PlayerState}
;; Empties the hand of the given player state
(define (clear-hand state)
  (apply-hand (thunk* '()) state))


#; {PlayerState Natural -> PlayerState}
;; Add the given number of points to the player state
(define (add-points state points)
  (define score (player-state-score state))
  (set-player-state-score state (+ score points)))


;; ========================================================================================
;; RENDERING FUNCTIONALITY
;; ========================================================================================

#; {PlayerState -> Image}
(define (render-player-state ps)
  (match-define [player-state score hand name _] ps)
  (define size (/ (*game-size*) 2))
  (define tiles-size (* (*game-size*) 2/3))
  (define name-score-text
    (if name
        (format "~a: ~a" (symbol->string name) (number->string score))
        (number->string score)))
  (define text-image (text name-score-text size 'black))
  (define tiles-image
    (parameterize ([*game-size* tiles-size])
      (render-tiles hand)))

  (above/align 'left text-image tiles-image))


;; ========================================================================================
;; UNIT TESTS
;; ========================================================================================

(module+ test
  (require rackunit)
  (define ps1 (player-state 0
                            (list (tile 'red 'square)
                                  (tile 'blue 'circle)
                                  (tile 'green 'star)
                                  (tile 'purple '8star)
                                  (tile 'yellow 'clover)
                                  (tile 'orange 'diamond))
                            'andrey
                            #f))
  (define sample-tiles (list (tile 'red 'square)
                             (tile 'blue 'square)
                             (tile 'green 'square)
                             (tile 'purple 'square)
                             (tile 'orange 'square)
                             (tile 'yellow 'square)
                             (tile 'red 'circle))))


(module+ test
  (test-equal?
   "remove tile list from hand"
   (remove-from-hand ps1
                     (list (tile 'red 'square)
                           (tile 'green 'star)))
   (player-state 0
                 (list (tile 'blue 'circle)
                                (tile 'purple '8star)
                                (tile 'yellow 'clover)
                                (tile 'orange 'diamond))
                 'andrey
                 #f))
  (test-equal?
   "remove empty tile list from hand"
   (remove-from-hand ps1 '())
   ps1)
  
  (test-equal?
   "make player state with empty tiles"
   (make-player-state '())
   (player-state 0 '() #f #f))
  
  (test-equal?
   "make player state with some tiles"
   (make-player-state (list (tile 'red 'square)
                            (tile 'blue 'circle)
                            (tile 'green 'star)
                            (tile 'purple '8star)
                            (tile 'yellow 'clover)
                            (tile 'orange 'diamond)))
   (player-state 0
                 (list (tile 'red 'square)
                       (tile 'blue 'circle)
                       (tile 'green 'star)
                       (tile 'purple '8star)
                       (tile 'yellow 'clover)
                       (tile 'orange 'diamond))
                 #f
                 #f))
  
  (test-values-equal?
   "clear empty hand"
   (clear-hand (make-player-state '()))
   (player-state 0 '() #f #f))
  
  (test-values-equal?
   "clear non-empty hand"
   (clear-hand ps1)
   (player-state (player-state-score ps1)
                 '()
                 'andrey
                 #f)))
