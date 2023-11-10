#lang racket

(require Q/Common/game-state)
(require Q/Common/turn-action)

(require Q/Player/strategy)

(provide compound%)

;; A "compound" strategy is a strategy that combines two strategies,
;; delegating to the first and then the second. If the first strategy
;; produces (pass), then the result of the compound strategy is the
;; result of the second.
(define compound%
  (class* object% (player-strategy<%>)
    (super-new)
    (init-field strategy0)
    (init-field strategy1)

    (define/public (choose-action pub-state)
      (define action0 (send strategy0 choose-action pub-state))
      (if (pass? action0)
          (send strategy1 choose-action pub-state)
          action0))))
