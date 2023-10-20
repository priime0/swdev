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

    (define/override (get-compare-accessor-list)
      (list (cons < posn-row)
            (cons < posn-column)))))
