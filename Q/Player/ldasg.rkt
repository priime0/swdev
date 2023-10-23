#lang racket

(require threading)

(require Q/Common/map)
(require Q/Common/turn-info)

(require Q/Player/strategy)
(require Q/Player/dag)

;; The "less dumb and still greedy" strategy selects the smallest placeable tile from the player's
;; hand, sorted lexicographically, and placed in the position with the most neighbors, breaking ties
;; with row-column order for coordinates.
(define ldasg%
  (class* dag% (player-strategy<%>)
    (super-new)

    (define/override (get-compare-accessor-list info)
      (match-define [turn-info _ _ _ board _] info)
      (cons (cons > (lambda~>> (adjacent-tiles board) length))
            (super get-compare-accessor-list info)))))

(module+ test
  (require rackunit)
  (require Q/Common/turn-info)
  (require Q/Common/player-state)
  (require Q/Common/data/tile)
  (require Q/Common/data/posn)
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

  (define ldasg1 (new ldasg%))

  (test-equal?
   "choose a simple action"
   (send ldasg1 choose-action turn-info-1)
   (place (list (placement (posn -1 0)
                           (tile 'green 'diamond))))))
