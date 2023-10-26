#lang racket

(require threading)

(require Q/Common/map)
(require Q/Common/game-state)

(require Q/Player/strategy)
(require Q/Player/dag)

;; The "less dumb and still greedy" strategy selects the smallest placeable tile from the player's
;; hand, sorted lexicographically, and placed in the position with the most neighbors, breaking ties
;; with row-column order for coordinates.
(define ldasg%
  (class* dag% (player-strategy<%>)
    (super-new)

    (define/override (get-compare-accessor-list pub-state)
      (match-define [game-state board _ _] pub-state)
      (cons (cons > (lambda~>> (adjacent-tiles board) length))
            (super get-compare-accessor-list pub-state)))))

(module+ test
  (require rackunit)
  (require Q/Common/game-state)
  (require Q/Common/player-state)
  (require Q/Common/map)
  (require Q/Common/data/tile)
  (require Q/Common/data/turn-action)
  (require Q/Common/data/posn)
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
       'lucas
       0
       (list
        (tile 'yellow 'clover)
        (tile 'green 'diamond)
        (tile 'yellow 'square)
        (tile 'red 'clover)
        (tile 'blue 'diamond)
        (tile 'purple 'circle)))
      (cons 'andrey 0)
      (cons 'luke 0))))

  (define pub-state-2
    (game-state
     (make-board (tile 'yellow '8star))
     6
     (list
      (player-state
       'lucas
       0
       (list
        (tile 'green 'clover)
        (tile 'green 'diamond)
        (tile 'green 'square)
        (tile 'red 'clover)
        (tile 'blue 'diamond)
        (tile 'purple 'circle)))
      (cons 'andrey 0)
      (cons 'luke 0))))

  (define pub-state-3
    (game-state
     (make-board (tile 'yellow '8star))
     5
     (list (player-state
            'lucas
            0
            (list
             (tile 'green 'clover)
             (tile 'green 'diamond)
             (tile 'green 'square)
             (tile 'red 'clover)
             (tile 'blue 'diamond)
             (tile 'purple 'circle)))
           (cons 'andrey 0)
           (cons 'luke 0))))

  (define dag-1 (new dag%)))

(module+ test
  (test-equal?
   "choose a simple action"
   (send dag-1 choose-action pub-state-1)
   (place (list (placement (posn -2 0)
                           (tile 'red 'clover)))))

  (test-equal?
   "can't place, so exchange"
   (send dag-1 choose-action pub-state-2)
   (exchange))

  (test-equal?
   "can't place or exchange, so pass"
   (send dag-1 choose-action pub-state-3)
   (pass)))
