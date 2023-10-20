#lang racket

(require Q/Common/turn-info)
(require Q/Common/data/turn-action)

(require Q/Player/strategy)

;; An "iterative strategy" contains some given strategy, accumulating an action by continuously
;; applying the strategy until the next possible accumulated action is invalid.
(define itstrat%
  (class* object% (player-strategy<%>)
    (init s)

    (define strat s)

    (super-new)

    (define/public (choose-action info)
      ;; generative: produces an accumulated turn action
      ;; terminates: when the last action is not a placement, or the produced new action is not
      ;;             valid. Continues iterating until the hand is empty -- the hand is finite, so it
      ;;             will terminate.
      (let/ec return
        (let loop ([action^ (pass)]
                   [info^ info])
          (define action  (send strat choose-action info^))
          (define action+ (combine-actions action^ action))

          (unless (valid-action? info action+)
            (return action^))

          (unless (place? action)
            (return action+))

          (define pment (first (place-placements action)))
          (define info+ (update-turn-info info^ pment))
          (loop action+ info+))))))
