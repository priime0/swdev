#lang racket

(require racket/generic)

(require struct-plus-plus)
(require 2htdp/image)

(require Q/Common/config)
(require Q/Common/data/tile)
(require Q/Common/util/list)
(require Q/Common/util/test)
(require Q/Common/interfaces/serializable)

(provide
 player-id?
 player-state++
 valid-hand?
 set-player-state-score
 set-player-state-hand
 hash->player-state++
 (struct-out player-state)
 (contract-out
  [make-player-state
   (->i ([id  player-id?]
         [hnd (listof tile?)])
        #:pre/name (hnd)
        "hand must be valid!"
        (valid-hand? hnd)
        [result player-state?])]
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
  [player-state->pair
   (-> player-state?
       (cons/c player-id? natural?))]
  [render-player-state
   (-> player-state?
       image?)]))

#; {type PlayerId = Symbol}
;; A PlayerId represents a _unique_ identifier for a player during a game.
(define player-id? symbol?)

#; {[Listof Tile] -> Boolean}
;; Does the given hand have at most *hand-size* tiles?
(define (valid-hand? hand)
  (<= (length hand) (*hand-size*)))

#; {type PlayerState = (player-state PlayerId Natural [Listof Tile])}
;; A PlayerState represents a participating player's state during any instant of time, containing
;; the points the player accrued during the game, along with the tiles in their hand during a move.
;; INVARIANT: The hand of a player state has a length L such that 0 ≤ L ≤ (*hand-size*).
(struct++ player-state
          ([id         player-id?]
           [(score 0)  natural?]
           [hand       (and/c (listof tile?)
                              valid-hand?)])
          #:transparent
          #:methods gen:serializable
          [(define/generic ->jsexpr* ->jsexpr)
           (define (->jsexpr ps)
             (match-define [player-state _id score hand] ps)
             (hash 'score score
                   'tile* (map ->jsexpr* hand)))])

#; {([Listof Tile] -> [Listof Tile]) PlayerState -> PlayerState}
(define (apply-hand f ps)
  (set-player-state-hand ps (f (player-state-hand ps))))

#; { PlayerState [Listof Tile] -> PlayerState }
;; Adds the given tiles to the hand of this player state.
(define (add-to-hand ps new-tiles)
  (apply-hand (curryr append new-tiles) ps))

#; {PlayerId [Listof Tile] -> PlayerState}
;; Creates a player with the given player id, hand, and a default score of 0.
(define (make-player-state id hand)
  (player-state++ #:id id #:hand hand))


#; {JPlayer -> PlayerState}
(define (hash->player-state++ jp)
  (player-state '|0|
                (hash-ref jp 'score)
                (map hash->tile++ (hash-ref jp 'tile*))))

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


#; {PlayerState -> [Pairof PlayerId Natural]}
(define (player-state->pair ps)
  (match-define [player-state id score _] ps)
  (cons id score))


#; {PlayerState -> Image}
(define (render-player-state ps)
  (match-define [player-state id score hand] ps)
  (define size (/ (*game-size*) 2))
  (define tiles-size (* (*game-size*) 2/3))
  (define text-image (text (~a id ": " score) size 'black))
  (define tiles-image
    (parameterize ([*game-size* tiles-size])
      (for/fold ([img empty-image])
                ([t (map render-tile hand)])
        (beside img t))))

  (above/align 'left text-image tiles-image))


(module+ test
  (require rackunit)
  (define ps1 (player-state 'lucas 0 (list (tile 'red 'square)
                                           (tile 'blue 'circle)
                                           (tile 'green 'star)
                                           (tile 'purple '8star)
                                           (tile 'yellow 'clover)
                                           (tile 'orange 'diamond))))
  (define sample-tiles (list (tile 'red 'square)
                             (tile 'blue 'square)
                             (tile 'green 'square)
                             (tile 'purple 'square)
                             (tile 'orange 'square)
                             (tile 'yellow 'square)
                             (tile 'red 'circle))))


(module+ test
  (test-true
   "is a player-id"
   (player-id? 'lucas))
  
  (test-false
   "is not a player-id"
   (player-id? "andrey"))
  
  (test-true
   "valid hand"
   (parameterize ([*hand-size* 6])
     (valid-hand? '())))
  
  (test-false
   "invalid hand"
   (parameterize ([*hand-size* 6])
     (valid-hand? (list (tile 'yellow 'square)
                        (tile 'blue 'square)
                        (tile 'red 'square)
                        (tile 'purple 'square)
                        (tile 'green 'square)
                        (tile 'orange 'square)
                        (tile 'orange 'circle)))))
  (test-equal?
   "remove tile list from hand"
   (remove-from-hand ps1 (list (tile 'red 'square)
                               (tile 'green 'star)))
   (player-state 'lucas 0 (list (tile 'blue 'circle)
                                (tile 'purple '8star)
                                (tile 'yellow 'clover)
                                (tile 'orange 'diamond))))
  (test-equal?
   "remove empty tile list from hand"
   (remove-from-hand ps1 '())
   ps1)
  
  (test-equal?
   "make player state with empty tiles"
   (make-player-state 'andrey '())
   (player-state 'andrey 0 '()))
  
  (test-equal?
   "make player state with some tiles"
   (make-player-state 'lucas (list (tile 'red 'square)
                                   (tile 'blue 'circle)
                                   (tile 'green 'star)
                                   (tile 'purple '8star)
                                   (tile 'yellow 'clover)
                                   (tile 'orange 'diamond)))
   (player-state 'lucas 0 (list (tile 'red 'square)
                                (tile 'blue 'circle)
                                (tile 'green 'star)
                                (tile 'purple '8star)
                                (tile 'yellow 'clover)
                                (tile 'orange 'diamond))))
  
  (test-equal?
   "player state to pair"
   (player-state->pair (make-player-state 'can '()))
   (cons 'can 0))
  
  (test-equal?
   "player state with non zero score to pair"
   (player-state->pair (player-state 'nishil 10 '()))
   (cons 'nishil 10))
  
  (test-equal?
   "player state with non-empty hand"
   (player-state->pair (make-player-state 'jamie (list (tile 'red 'square)
                                                       (tile 'blue 'circle)
                                                       (tile 'green 'star)
                                                       (tile 'purple '8star)
                                                       (tile 'yellow 'clover)
                                                       (tile 'orange 'diamond))))
   (cons 'jamie 0))
  
  (test-values-equal?
   "clear empty hand"
   (clear-hand (make-player-state 'rohan '()))
   (player-state 'rohan 0 '()))
  
  (test-values-equal?
   "clear non-empty hand"
   (clear-hand ps1)
   (player-state (player-state-id ps1)
                 (player-state-score ps1)
                 '()))
  
  (test-values-equal?
   "refill empty hand"
   (refill-hand (make-player-state 'andrey '()) sample-tiles)
   (make-player-state 'andrey (take sample-tiles (*hand-size*))))
  
  (test-values-equal?
   "refill non-empty hand"
   (refill-hand (make-player-state 'andrey (list (tile 'red 'square))) sample-tiles)
   (make-player-state 'andrey
                      (append (list (tile 'red 'square))
                              (take sample-tiles (- (*hand-size*) 1))))))