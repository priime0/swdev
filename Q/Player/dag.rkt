#lang racket

(require Q/Common/data/posn)

(require Q/Player/strategy)
(require Q/Player/greedy-select-strategy)

(provide dag%)

;; The "dumb and greedy" strategy selects the smallest placeable tile from the player's hand, sorted
;; lexicographically, and placed at the row-column order in smallest position for the tile on the
;; board.
(define dag%
  (class* greedy-select-strategy% (player-strategy<%>)
    (super-new)

    (define/override (get-compare-accessor-list _info)
      (list (cons < posn-row)
            (cons < posn-column)))))


(module+ test
  (require rackunit)
  (require Q/Common/turn-info)
  (require Q/Common/player)
  (require Q/Common/map)
  (require Q/Common/data/tile)
  (require Q/Common/data/turn-action)

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

  (define dag1 (new dag%))

  (test-equal?
   "choose a simple action"
   (send dag1 choose-action turn-info-1)
   (place (list (placement (posn -1 0)
                           (tile 'green 'diamond))))))
