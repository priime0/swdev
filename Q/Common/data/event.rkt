#lang racket

(require struct-plus-plus)

(require Q/Common/player)
(require Q/Common/data/turn-action)

(provide
 (struct-out turn)
 (struct-out round-end)
 (struct-out banish)
 (struct-out game-end)
 game-event?
 history?)

#; {type GameEvent = (U (turn PlayerId TurnAction)
                        (round-end)
                        (banish)
                        (game-end))}
;; A GameEvent represents a possible event occurring in the game, where `turn` represents a turn a
;; player made, `round-end` represents the end of a round, `banish` represents the removal of a
;; player by disconnect or kicking, and `game-end` signifies the end of a game.
(struct++ turn
          ([id player-id?]
           [action turn-action?])
          #:transparent)
(struct   banish      (id final-score) #:transparent)
(struct   round-end   ()               #:transparent)
(struct   game-end    ()               #:transparent)

#; {Any -> Boolean}
(define (game-event? a)
  ((disjoin turn? round-end? banish? game-end?) a))

#; {type History = [Listof GameEvent]}
;; A History represents an audit log of significant game events for observers, referees, and
;; players.
;; INVARIANT: There can exist at most one `game-end` instance, and if it exists, then it must be the
;; first element in the list.
(define history?
  (listof game-event?))
