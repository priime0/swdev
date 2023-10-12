#lang racket

(require struct-plus-plus)

(require Q/Common/data/posn)
(require Q/Common/data/tile)

(provide
 (struct-out place-tile)
 (struct-out exchange)
 (struct-out pass)
 turn-action?
 (struct-out placement))


#; {type TilePlacement = (placement Posn Tile)}
;; A TilePlacement represents a request from the player to place the given tile at the given
;; position.
(struct++ placement
          ([posn posn?]
           [tile tile?])
          #:transparent)


#; {type TurnAction = (U (place-tile [Listof TilePlacement])
                         (exchange)
                         (pass)
                         (round-end))}
;; A TurnAction represents a possible action during a player turn, and is one of:
;; - A placement of tiles onto the board at the corresponding locations in the given order
;; - An exchange of all tiles in a player's hand
;; - Skipping a player's turn (withdrawing from performing any actions)
(struct++ place-tile ([placements (listof placement?)]) #:transparent)
(struct   exchange   ()                                 #:transparent)
(struct   pass       ()                                 #:transparent)


#; {Any -> Boolean}
(define (turn-action? a)
  ((disjoin place-tile? exchange? pass?) a))