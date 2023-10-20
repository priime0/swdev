#lang racket

(require threading)

(require Q/Common/map)

(require Q/Player/strategy)
(require Q/Player/dag)

;; The "less dumb and still greedy" strategy selects the smallest placeable tile from the player's
;; hand, sorted lexicographically, and placed in the position with the most neighbors, breaking ties
;; with row-column order for coordinates.
(define ldasg%
  (class* dag% (player-strategy<%>)
    (super-new)

    (define/override (get-compare-accessor-list)
      (cons (cons > (lambda~> adjacent-tiles length))
            (super get-compare-accessor-list)))))
