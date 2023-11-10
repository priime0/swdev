#lang racket

(require racket/generic)

(require struct-plus-plus)

(require Q/Common/posn)
(require Q/Common/tile)
(require Q/Common/interfaces/serializable)
(require Q/Lib/contracts)

(provide
 (struct-out place)
 place-placements
 (struct-out exchange)
 (struct-out pass)
 turn-action?
 hash->placement
 (struct-out placement))


#; {type TilePlacement = (placement Posn Tile)}
;; A TilePlacement represents a request from the player to place the given tile at the given
;; position.
(struct placement (posn tile)
  #:transparent
  #:methods gen:serializable
  [(define/generic ->jsexpr* ->jsexpr)
   (define (->jsexpr pment)
     (match-define [placement posn tile] pment)
     (hash 'coordinate (->jsexpr* posn)
           '1tile      (->jsexpr* tile)))])


(define (hash->placement h)
  (define p (hash->posn (hash-ref h 'coordinate)))
  (define t (hash->tile (hash-ref h '1tile)))
  (placement p t))


#; {type TurnAction = (U (place-tile [Listof TilePlacement])
                         (exchange)
                         (pass))}
;; A TurnAction represents a possible action during a player turn, and is one of:
;; - A placement of tiles onto the board at the corresponding locations in the given order
;; - An exchange of all tiles in a player's hand
;; - Skipping a player's turn (withdrawing from performing any actions)
(struct   place     (placements) #:transparent)
(struct   exchange  ()           #:transparent)
(struct   pass      ()           #:transparent)


#; {Any -> Boolean}
(define (turn-action? a)
  ((disjoin place? exchange? pass?) a))


(module+ test
  (require rackunit))

(module+ test

  (test-true
   "valid place turn action"
   (turn-action? (place (list (placement (posn 0 0) (tile 'red 'square))))))

  (test-true
   "valid turn action"
   (turn-action? (exchange)))

  (test-true
   "valid pass turn action"
   (turn-action? (pass)))

  (test-false
   "invalid turn action"
   (turn-action? '())))
