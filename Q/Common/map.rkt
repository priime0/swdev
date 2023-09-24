#lang racket

(require struct-plus-plus)
(require data/gvector)

#; {type Board = [GVector [GVector [Maybe Tile]]]}
;; A Board is a grid of Tiles or empty spaces that can grow in any direction.
;; INVARIANT: The grid has a buffer of #f values that surround the in-play tiles. 
(struct++ board
          ([grid gvector?])
          #:transparent)

#; {[Maybe Tile] -> Board}
;; Create a 3x3 board with the given [Maybe Tile] placed at the position (1, 1)
(define (make-board maybe-tile)
  (board++ #:grid
           (gvector (gvector #f #f #f)
                    (gvector #f maybe-tile #f)
                    (gvector #f #f #f))))

