#lang racket

(require (rename-in (only-in lazy define)
                    [define define/lazy]))

(require struct-plus-plus)

(require Q/Common/player)
(require Q/Common/data/turn-action)
(require Q/Common/util/list)

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
#; {Any -> Boolean}
(define (history? a)
  (define (no-game-ends? l)
    (not (member? (game-end) l)))

  (define/lazy at-most-one-end?
    (or (no-game-ends? a)
        (and (game-end? (first a))
             (no-game-ends? (rest a)))))

  (and (list? a)
       (andmap game-event? a)
       at-most-one-end?))


(module+ test
  (require rackunit)

  (define turn1 (turn 'lucas (exchange)))
  (define history1 (list (banish 'andrey 10) (round-end) turn1 (round-end))))

(module+ test

  (test-true
   "valid game event"
   (game-event? (game-end)))

  (test-true
   "valid turn game event"
   (game-event? turn1))

  (test-false
   "invalid game event"
   (game-event? '()))

  (test-true
   "valid history"
   (history? history1))

  (test-true
   "valid history with game-end"
   (history? (cons (game-end) history1)))

  (test-false
   "invalid history with game-end in last position"
   (history? (append history1 (list (game-end)))))

  (test-false
   "invalid history with two game-ends"
   (history? (append (cons (game-end) history1)
                     (list (game-end))))))

