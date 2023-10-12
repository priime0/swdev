#lang racket

(require (rename-in data/functor
                    [map fmap]))

(require struct-plus-plus)

(require Q/Common/config)
(require Q/Common/data/tile)
(require Q/Common/util/list)

(provide
 player-id?
 player-state++
 valid-hand?
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
