#lang racket

(require Q/Common/config)
(require Q/Common/game-state)
(require Q/Common/player-state)
(require Q/Common/turn-action)
(require Q/Lib/list)

(require Q/Player/strategy)

(provide greedy-select-strategy%)

(define greedy-select-strategy%
  (class* object% (player-strategy<%>)
    (super-new)

    #; {-> [Listof [Pair Procedure Procedure]]}
    ;; Produces an association list of binary comparison functions and accessor functions, so lists
    ;; can be stable-sorted by extracting values from list elements with the accessor functions, and
    ;; comparing them with the binary comparison functions.
    (abstract get-compare-accessor-list)

    #; {TurnInfo -> TurnAction}
    (define/public (choose-action pub-state)
      (match-define [game-state board tiles* [cons state others]] pub-state)

      (define first-maybe-tile-pair
        (choose-tile (player-state-hand state) board))

      (cond
        [(cons? first-maybe-tile-pair)
         (match-define [cons t posns] first-maybe-tile-pair)
         (define compare-accessor-list
           (send this get-compare-accessor-list pub-state))
         (define posns-sort
           (sort-by compare-accessor-list))
         (choose-placement t posns posns-sort)]
        [(>= tiles* (length (player-state-hand state)))
         (exchange)]
        [else
         (pass)]))))
