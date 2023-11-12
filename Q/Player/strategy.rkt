#lang racket

(require Q/Common/game-state)
(require Q/Common/turn-action)


(provide
 player-strategy<%>)


;; A PlayerStrategy is an interface that represents the functionality that any player strategy will
;; have to support, namely, choosing an action to perform given some turn information.
(define player-strategy<%>
  (interface ()
    #; {PlayerStrategy PublicState -> TurnAction}
    ;; Given some turn information for the player, produce an action to influence the game
    ;; state.
    [choose-action (->m pub-state/c turn-action?)]))




