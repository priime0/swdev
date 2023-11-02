#lang racket

(require racket/generic)
(require racket/lazy-require)

(require struct-plus-plus)
(require 2htdp/image)

(require Q/Common/config)
(require Q/Common/tile)
(require Q/Common/util/list)
(require Q/Common/util/test)
(require Q/Common/interfaces/serializable)
(require Q/Common/interfaces/playable)

(provide
 player-state++
 set-player-state-score
 set-player-state-hand
 hash->player-state++
 (struct-out player-state)
 (contract-out
  [make-player-state
   (-> (listof tile?)
       (is-a?/c playable<%>)
       player-state?)]
  [add-to-hand
   (-> player-state? (listof tile?)
       player-state?)]
  [deficit
    (-> player-state?
        natural?)]
  [remove-from-hand
   (->i ([ps  player-state?]
         [tls (listof tile?)])
        #:pre/name (ps tls)
        "hand must contain given tiles!"
        (contains-all? (player-state-hand ps) tls)
        [result player-state?])]
  [clear-hand
   (-> player-state?
       (values player-state?
               (listof tile?)))]
  [refill-hand
   (-> player-state?
       (listof tile?)
       player-state?)]
  [add-points
   (-> player-state?
       natural?
       player-state?)]
  [render-player-state
   (-> player-state?
       image?)]))

#; {type PlayerState = (player-state Natural [Listof Tile] Playable)}
;; A PlayerState represents a participating player's state during any instant of time, containing
;; the points the player accrued during the game, along with the tiles in their hand during a move.
;; INVARIANT: The hand of a player state has a length L such that 0 ≤ L ≤ (*hand-size*).
(struct++ player-state
          ([(score 0)       natural?]
           [hand            (listof tile?)]
           [(player #f)     (or/c (is-a?/c playable<%>) false?)])
          #:transparent
          #:methods gen:serializable
          [(define/generic ->jsexpr* ->jsexpr)
           (define (->jsexpr ps)
             (match-define [player-state score hand _] ps)
             (hash 'score score
                   'tile* (map ->jsexpr* hand)))])

#; {([Listof Tile] -> [Listof Tile]) PlayerState -> PlayerState}
(define (apply-hand f ps)
  (set-player-state-hand ps (f (player-state-hand ps))))

#; { PlayerState [Listof Tile] -> PlayerState }
;; Adds the given tiles to the hand of this player state.
(define (add-to-hand ps new-tiles)
  (apply-hand (curryr append new-tiles) ps))

#; {[Listof Tile] -> PlayerState}
;; Creates a player with the given player id, hand, and a default score of 0.
(define (make-player-state hand [playable #f])
  (player-state++ #:hand hand #:player playable))


#; {JPlayer -> PlayerState}
(define (hash->player-state++ jp)
  (define score (hash-ref jp 'score))
  (define hand (map hash->tile++ (hash-ref jp 'tile*)))
  (player-state++ #:score score
                  #:hand  hand))

#; {PlayerState [Listof Tile] -> PlayerState}
;; Removes the given tiles from the hand of the given player state.
(define (remove-from-hand state tiles)
  (apply-hand (curry remove-from tiles) state))

#; {PlayerState -> PlayerState}
;; Empties the hand of the given player state
(define (clear-hand state)
  (apply-hand (thunk* '()) state))

#; {PlayerState -> Natural}
;; Computes the deficit in the hand
(define (deficit state)
  (define hand        (player-state-hand state))
  (define hand-length (length hand))
  (- (*hand-size*) hand-length))


#; {PlayerState [Listof Tiles] -> PlayerState}
;; Replenishes the hand of the given player state from the given list of tiles, returning
;; the new player state, leaving the tiles unmodified.
(define (refill-hand state tiles)
  (define missing     (deficit state))
  (define available   (length tiles))
  (define vendable    (min missing available))

  (add-to-hand state (take tiles vendable)))

#; {PlayerState Natural -> PlayerState}
;; Add the given number of points to the player state
(define (add-points state points)
  (define score (player-state-score state))
  (set-player-state-score state (+ score points)))

#; {PlayerState -> Image}
(define (render-player-state ps)
  (match-define [player-state score hand _] ps)
  (define size (/ (*game-size*) 2))
  (define tiles-size (* (*game-size*) 2/3))
  (define text-image (text score size 'black))
  (define tiles-image
    (parameterize ([*game-size* tiles-size])
      (for/fold ([img empty-image])
                ([t (map render-tile hand)])
        (beside img t))))

  (above/align 'left text-image tiles-image))


(module+ test
  (require rackunit)
  (define ps1 (player-state 0
                            (list (tile 'red 'square)
                                  (tile 'blue 'circle)
                                  (tile 'green 'star)
                                  (tile 'purple '8star)
                                  (tile 'yellow 'clover)
                                  (tile 'orange 'diamond))
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
                 #f))
  (test-equal?
   "remove empty tile list from hand"
   (remove-from-hand ps1 '())
   ps1)
  
  (test-equal?
   "make player state with empty tiles"
   (make-player-state '())
   (player-state 0 '() #f))
  
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
                 #f))
  
  (test-values-equal?
   "clear empty hand"
   (clear-hand (make-player-state '()))
   (player-state 0 '() #f))
  
  (test-values-equal?
   "clear non-empty hand"
   (clear-hand ps1)
   (player-state (player-state-score ps1)
                 '()
                 #f))
  
  (test-values-equal?
   "refill empty hand"
   (refill-hand (make-player-state '()) sample-tiles)
   (make-player-state (take sample-tiles (*hand-size*))))
  
  (test-values-equal?
   "refill non-empty hand"
   (refill-hand (make-player-state (list (tile 'red 'square))) sample-tiles)
   (make-player-state (append (list (tile 'red 'square))
                              (take sample-tiles (- (*hand-size*) 1))))))
