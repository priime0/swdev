#lang racket

(require racket/class)

(require Q/Common/game-state)
(require Q/Common/map)
(require Q/Common/interfaces/playable)
(require Q/Common/data/turn-action)



;; This Player is a concrete, stateless implementation of Playable with a name and a Strategy to act
;; with.
(define player%
  (class* object% (playable<%>)
    (init-field id)
    (init-field strategy)
    (super-new)

    (define/public (name)
      (symbol->string id))

    (define/public (setup board tiles)
      this%)

    (define/public (take-turn pub-state)
      (unless (protected-board/c (game-state-board pub-state))
        (error 'take-turn
               "invalid board received before taking turn"))

      (send strategy choose-action pub-state))

    (define/public (new-tiles tiles)
      (void))

    (define/public (win won?)
      (void))))
