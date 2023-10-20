#lang racket

(require Q/Common/config)
(require Q/Common/turn-info)
(require Q/Common/player)
(require Q/Common/data/turn-action)
(require Q/Common/util/list)

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
    (define/public (choose-action info)
      (match-define [turn-info state _scores _history board tiles*] info)

      (define first-maybe-tile-pair
        (choose-tile (player-state-hand state) board))

      (cond
        [(cons? first-maybe-tile-pair)
         (match-define [cons t posns] first-maybe-tile-pair)
         (define compare-accessor-list
           (send this get-compare-accessor-list info))
         (define posns-sort
           (sort-by compare-accessor-list))
         (choose-placement t posns posns-sort)]
        [(>= tiles* (*hand-size*))
         (exchange)]
        [else
         pass]))))