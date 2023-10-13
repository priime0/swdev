#lang racket

(require (rename-in data/functor
                    [map fmap]))

(require struct-plus-plus)

(require Q/Common/config)
(require Q/Common/data/tile)
(require Q/Common/util/list)
(require Q/Common/util/test)

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
  [is-player?
   (-> player-id? player-state? boolean?)]
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
       (values player-state?
               (listof tile?)))]
  [player-state->pair
   (-> player-state?
       (cons/c player-id? natural?))]))

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
          #:methods gen:functor
          [(define (map f x)
             (match-define [player-state id score hand] x)
             (player-state id score (f hand)))])


#; {PlayerId [Listof Tile] -> PlayerState}
;; Creates a player with the given player id, hand, and a default score of 0.
(define (make-player-state id hand)
  (player-state++ #:id id #:hand hand))


#; {JPlayer -> PlayerState}
(define (hash->player-state++ jp)
  (make-player-state '|0|
                     (hash-ref jp 'score)
                     (hash-ref jp 'tile*)))


#; {PlayerId PlayerState -> Boolean}
;; Does the given player id correspond to this player?
(define (is-player? id ps)
  (symbol=? id (player-state-id ps)))


#; {PlayerState [Listof Tile] -> PlayerState}
;; Removes the given tiles from the hand of the given player state.
(define (remove-from-hand state tiles)
  (fmap (curry remove-from tiles) state))


#; {PlayerState -> (values PlayerState [Listof Tile])}
;; Empties the hand of the given player state, returning the new empty-handed player state,
;; and its former tiles.
(define (clear-hand state)
  (values (fmap (thunk* '()) state)
          (player-state-hand state)))


#; {PlayerState [Listof Tiles] -> (values PlayerState [Listof Tiles])}
;; Replenishes the hand of the given player state from the given list of tiles, returning
;; the new player state and the remaining tiles.
(define (refill-hand state tiles)
  (define hand        (player-state-hand state))
  (define hand-length (length hand))
  (define missing     (- (*hand-size*) hand-length))
  (define available   (length tiles))
  
  (define-values (refill tiles+)
    (split-at tiles
              (min missing available)))
  (define hand+ (append hand refill))
  
  (define state+ (set-player-state-hand state hand+))
  (values state+ tiles+))


#; {PlayerState -> [Pairof PlayerId Natural]}
(define (player-state->pair ps)
  (match-define [player-state id score _] ps)
  (cons id score))


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
  
  (test-true
   "is player"
   (is-player? 'lucas ps1))
  
  (test-false
   "is not player"
   (is-player? 'andrey ps1))
  
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
   (values (player-state 'rohan 0 '())
           '()))
  
  (test-values-equal?
   "clear non-empty hand"
   (clear-hand ps1)
   (values (player-state (player-state-id ps1)
                         (player-state-score ps1)
                         '())
           (player-state-hand ps1)))
  
  (test-values-equal?
   "refill empty hand"
   (refill-hand (make-player-state 'andrey '()) sample-tiles)
   
   (values (make-player-state 'andrey (take sample-tiles (*hand-size*)))
           (drop sample-tiles (*hand-size*))))
  
  (test-values-equal?
   "refill non-empty hand"
   (refill-hand (make-player-state 'andrey (list (tile 'red 'square))) sample-tiles)
   
   (values (make-player-state 'andrey
                              (append (list (tile 'red 'square))
                                      (take sample-tiles (- (*hand-size*) 1))))
           (drop sample-tiles (- (*hand-size*) 1)))))
