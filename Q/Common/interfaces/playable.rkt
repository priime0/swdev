#lang racket

(require racket/class)

(require Q/Common/map)
(require Q/Common/game-state)
(require Q/Common/data/tile)
(require Q/Common/data/turn-action)


(provide playable<%>)


#; {interface Playable}
;; Represents the public player API for interacting with a referee. Any player must implement
;; this functionality to play a game of Q.
(define playable<%>
  (interface ()
    #; {Playable -> String}
    ;; Retrieve the name of the player
    [name      (->m string?)]
    #; {Playable Board Tile -> Void}
    ;; Set up the player's knowledge of the game
    [setup     (->m protected-board/c (listof tile?) void?)]
    #; {Playable TurnInfo -> TurnAction}
    ;; Execute the player's strategy with the given turn information to produce an action.
    [take-turn (->m pub-state/c turn-action?)]
    #; {[Listof Tile] -> Void}
    ;; Give tiles to the player
    [new-tiles (->m (listof tile?) void?)]
    #; {Boolean -> Void}
    ;; Send information on whether the player won
    [win       (->m boolean? void?)]))
