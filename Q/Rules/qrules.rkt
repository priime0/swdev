#lang racket

(require (rename-in (only-in lazy define)
                    [define define/lazy]))

(require (rename-in data/functor
                    [map fmap]))

(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/data/turn-action)
(require Q/Common/config)
(require Q/Common/map)
(require Q/Common/turn-info)
(require Q/Common/player)
(require Q/Common/util/list)


(provide
 (contract-out
  [valid-placement?
   (-> board?
       placement?
       boolean?)]
  [valid-action?
   (-> turn-info?
       turn-action?
       boolean?)]))

#; {Tile [Listof Tile] -> Boolean}
;; Does the given tile share the same color or (inclusive) same color as every tile in the given
;; list?
(define (valid-tile-sequence? tile tiles)
  ((disjoin tiles-equal-color? tiles-equal-shape?)
   (cons tile tiles)))


#; {Board TilePlacement -> Boolean}
;; Is the given placement valid in the game of Q?
;; A valid Q position is one where the left and right adjacent tiles, if any, share the same shape
;; or (inclusive) the same color, and the up and down adjacent tiles, if any, share the same shape
;; or (inclusive) same color.
(define (valid-placement? board pment)
  (match-define [placement posn tile] pment)
  #; {[Listof Direction] -> [Listof Tile]}
  ;; Gets the existing tiles in the given directions.
  (define (adjacent-tiles dirs)
    (define tile-at^          (curry tile-at board))
    (define target-neighbors  (curry posn-neighbors/dirs posn))
    (filter-map tile-at^      (target-neighbors dirs)))
  
  (define row-adjacent-tiles (adjacent-tiles horizontal-axis))
  (define col-adjacent-tiles (adjacent-tiles vertical-axis))
  
  (define/lazy adjacent-tiles? (has-adjacent-tiles? board posn))
  (define/lazy matches-neighbors?
    (andmap (curry valid-tile-sequence? tile)
            (list row-adjacent-tiles col-adjacent-tiles)))
  
  (and adjacent-tiles? matches-neighbors?))


#; {TurnInfo TurnAction -> Boolean}
;; Is the given action on this turn valid?
(define (valid-action? info action)
  (match action
    [(place pments) (valid-place? info pments)]
    [(exchange)     (valid-exchange? info)]
    [(pass)         #t]))

#; {TurnInfo [Listof TilePlacement] -> Boolean}
;; Is the proposed placement for the given turn valid?
(define (valid-place? info placements)
  (match-define [turn-info [player-state _ _ hand] _ _ board _] info)

  (define in-hand?
    (contains-all? hand (map placement-tile placements)))
  (define aligned?
    (same-axis? (map placement-posn placements)))

  (and (pair? placements)
       aligned?
       (legal-placements? info placements)
       in-hand?))

(define (legal-placements? info placements)
  (match-define [turn-info [player-state _ _ hand] _ _ board _] info)
  (for/fold ([b^ board]
             #:result b^)
            ([pment placements]
             #:break (not b^))
    (and (valid-placement? b^ pment)
         (add-tile b^ pment))))


#; {TurnInfo -> Boolean}
;; Is an exchange a valid move for this turn?
(define (valid-exchange? info)
  (>= (turn-info-tiles-left info) (*hand-size*)))



(module+ test
  (require rackunit)

  (define example-board
    (fmap (thunk* (hash (posn 0 0) (tile 'red 'square)
                        (posn 1 0) (tile 'red 'circle)
                        (posn 0 1) (tile 'green 'square)
                        (posn 2 0) (tile 'red 'star)))
          (make-board (tile 'red 'square))))
  (define example-board2
    (fmap (thunk* (hash (posn 0 0)   (tile 'red 'square)
                        (posn 0 -1)  (tile 'green 'circle)
                        (posn -1 -1) (tile 'green 'square)
                        (posn 1 0)   (tile 'red 'star)
                        (posn 2 0)   (tile 'blue 'star)
                        (posn 2 1)   (tile 'blue '8star)))
          (make-board (tile 'red 'square)))))


(module+ test
  (test-true
   "valid tile sequence with same color"
   (valid-tile-sequence? (tile 'red 'square)
                         (list (tile 'red 'circle)
                               (tile 'red 'star))))

  (test-true
   "valid tile sequence with same shape"
   (valid-tile-sequence? (tile 'red 'square)
                         (list (tile 'blue 'square)
                               (tile 'green 'square))))

  (test-true
   "valid tile sequence with same color and shape"
   (valid-tile-sequence? (tile 'red 'square)
                         (list (tile 'red 'square)
                               (tile 'red 'square))))

  (test-false
   "invalid tile sequence with different color and shape"
   (valid-tile-sequence? (tile 'red 'square)
                         (list (tile 'blue 'circle)
                               (tile 'green 'star))))

  (test-true
   "board valid position for a valid tile at (1, 1)"
   (valid-placement? example-board (placement (posn 1 1) (tile 'green 'circle))))

  (test-false
   "board invalid position for an invalid tile at (1, 1)"
   (valid-placement? example-board (placement (posn 1 1) (tile 'green 'square))))

  (test-check
   "possible tile positions for a red clover"
   set=?
   (valid-tile-placements (tile 'red 'clover) example-board valid-placement?)
   (list (posn 0 -1) (posn -1 0) (posn 3 0) (posn 2 -1) (posn 2 1) (posn 1 -1)))

  (test-check
   "possible tile positions for a green square"
   set=?
   (valid-tile-placements (tile 'green 'square) example-board valid-placement?)
   (list (posn 0 -1) (posn -1 0) (posn -1 1) (posn 0 2)))

  (test-check
   "no possible tile positions for a blue 8star"
   set=?
   (valid-tile-placements (tile 'blue '8star) example-board valid-placement?)
   '()))
