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

(module+ test
  (require rackunit)
  (require Q/Common/turn-info)
  (require Q/Common/player)
  (require Q/Common/map)
  (require Q/Common/data/tile)
  (require Q/Common/data/posn)
  (require Q/Common/data/turn-action)
  (require Q/Player/dag)

  (define turn-info-1
    (turn-info
     (player-state
      'lucas
      0
      (list
       (tile 'yellow 'clover)
       (tile 'green 'diamond)
       (tile 'yellow 'square)
       (tile 'red 'clover)
       (tile 'blue 'diamond)
       (tile 'purple 'circle)))
     '((andrey . 0) (luke . 0))
     '()
     (make-board (tile 'orange 'diamond))
     17))

  (define dag (new dag%))
  (define iterative (new itstrat% [s dag]))

  (test-equal?
   "choose a simple iterative action"
   (send iterative choose-action turn-info-1)
   (place (list (placement (posn -1 0)
                           (tile 'green 'diamond))
                (placement (posn -2 0)
                           (tile 'blue 'diamond))))))
