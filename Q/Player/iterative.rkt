#lang racket

(require Q/Common/game-state)
(require Q/Common/turn-action)

(require Q/Player/strategy)

(provide iterative%)

;; An "iterative strategy" contains some given strategy, accumulating an action by continuously
;; applying the strategy until the next possible accumulated action is invalid.
(define iterative%
  (class* object% (player-strategy<%>)
    (super-new)
    (init-field strategy)

    (define/public (choose-action pub-state)
      ;; generative: produces an accumulated turn action
      ;; terminates: when the last action is not a placement, or the produced new action is not
      ;;             valid. Continues iterating until the hand is empty -- the hand is finite, so it
      ;;             will terminate.
      (let loop ([action^ (pass)]
                 [pub-state^ pub-state])
        (define action  (send strategy choose-action pub-state^))
        (define action+ (combine-actions action^ action))

        (cond [(not (turn-valid? pub-state action+)) action^]
              [(not (place? action))                 action+]
              [else
               (define pub-state+ (do-turn/action pub-state^ action))
               (loop action+ pub-state+)])))))

(module+ test
  (require rackunit)
  (require Q/Common/game-state)
  (require Q/Common/player-state)
  (require Q/Common/map)
  (require Q/Common/tile)
  (require Q/Common/turn-action)
  (require Q/Common/posn)
  (require Q/Player/ldasg)
  (require Q/Player/dag)
  (require threading)

  (define board-1
    (~>> (make-board (tile 'orange 'diamond))
         (add-tile _ (placement (posn -1 0) (tile 'red 'diamond)))
         (add-tile _ (placement (posn -1 -1) (tile 'blue 'diamond)))))

  (define pub-state-1
    (game-state
     board-1
     17
     (list
      (player-state
       0
       (list
        (tile 'yellow 'clover)
        (tile 'green 'diamond)
        (tile 'yellow 'square)
        (tile 'red 'clover)
        (tile 'blue 'diamond)
        (tile 'purple 'circle))
       #f)
      0
      0)))

  (define pub-state-2
    (game-state
     (make-board (tile 'yellow '8star))
     6
     (list
      (player-state
       0
       (list
        (tile 'green 'clover)
        (tile 'green 'diamond)
        (tile 'green 'square)
        (tile 'red 'clover)
        (tile 'blue 'diamond)
        (tile 'purple 'circle))
       #f)
      0
      0)))

  (define pub-state-3
    (game-state
     (make-board (tile 'yellow '8star))
     5
     (list (player-state
            0
            (list
             (tile 'green 'clover)
             (tile 'green 'diamond)
             (tile 'green 'square)
             (tile 'red 'clover)
             (tile 'blue 'diamond)
             (tile 'purple 'circle))
            #f)
           0
           0)))

  (define ldasg-1 (new ldasg%))
  (define it-1 (new iterative% [strategy ldasg-1]))
  (define dag-1 (new dag%))
  (define it-2 (new iterative% [strategy dag-1])))

(module+ test
  (test-equal?
   "choose a simple action"
   (send it-1 choose-action pub-state-1)
   (place (list
           (placement (posn -2 0) (tile 'red 'clover))
           (placement (posn -3 0) (tile 'yellow 'clover))
           (placement (posn -4 0) (tile 'yellow 'square)))))

  (test-equal?
   "can't place, so exchange"
   (send it-1 choose-action pub-state-2)
   (exchange))

  (test-equal?
   "can't place or exchange, so pass"
   (send it-1 choose-action pub-state-3)
   (pass))

  (test-equal?
   "choose a simple action"
   (send it-2 choose-action pub-state-1)
   (place (list
           (placement (posn -2 0) (tile 'red 'clover))
           (placement (posn -3 0) (tile 'yellow 'clover))
           (placement (posn -4 0) (tile 'yellow 'square)))))

  (test-equal?
   "can't place, so exchange"
   (send it-2 choose-action pub-state-2)
   (exchange))

  (test-equal?
   "can't place or exchange, so pass"
   (send it-2 choose-action pub-state-3)
   (pass)))
