#lang racket

(require (rename-in (only-in lazy define)
                    (define define/lazy)))

(require struct-plus-plus)
(require threading)

(require Q/Common/data/tile)
(require Q/Common/data/posn)

(provide
 board?

 (contract-out
  #:unprotected-submodule no-contract
  [make-board (-> tile? board?)]
  [add-tile
   (-> board?
       posn?
       tile?
       board?)]
  [valid-tile-placements
   (-> board?
       tile?
       (listof posn?))]))



#; {type Board = (board [HashTable Posn Tile])}
;; A Board represents a map, implemented as a hash table where the keys are 2D coordinates relative
;; to the root tile, and the values are the tiles at the given coordinate.
;; DEFINITION: Two tiles are considered _adjacent_ IFF WLOG the posns where the tiles are placed are
;;             neighbors.
;; INVARIANT 0: A Board is a connected graph.
;; INVARIANT 1: The Tile placed at (0, 0) is the *root* tile of the game, which is the only referee
;;              placed-tile. All other tiles have coordinates that are relative to this tile.
;; INVARIANT 2: No other tile will be placed on an existing tile -- that is, two tiles cannot share
;;              the same key/coordinates, and a tile cannot replace a tile that is already on the
;;              map.
;; INVARIANT 3: A position on the board is occupied by a tile IFF the position is a key in the hash
;;              map.
(struct++ board
          ([map hash?])
          #:transparent)


#; {Tile -> Board}
;; Create a Board with the given Tile placed at position (0, 0) -- the Tile represents the *root*
;; tile of a game, which is the only referee-placed tile.
(define (make-board root-tile)
  (define root-posn (posn 0 0))
  (board++ #:map (hash root-posn root-tile)))


#; {Board Posn Tile -> Board}
;; Places the new tile at the given posn on the board's map.
;; EXCEPT: Throws an error if the given position is invalid, preserving INVARIANT 0, 2, 3.
(define (add-tile board posn new-tile)
  (unless (posn-empty+has-adjacent? board posn)
    (error 'board-add-tile
           "posn ~a invalid position to place on board"
           posn))

  (define tiles-map (board-map board))
  (define tiles-map+ (hash-set tiles-map posn new-tile))
  (set-board-map board tiles-map+))


#; {Tile Board -> [Listof Posn]}
;; Produce a list of placements (posns) that are valid for this tile on the given board.
(define (valid-tile-placements tile board)
  (define all-open-posns (open-posns board))
  (define valid-posn? (curry valid-placement? board tile))
  (filter valid-posn? all-open-posns))


#; {Board Posn -> [Maybe Tile]}
;; Gets the tile at `posn` if it exists, otherwise #f
(define (tile-at board posn)
  (define tiles-map (board-map board))
  ((conjoin hash-has-key? hash-ref) 
   tiles-map posn))


#; {Board Posn -> Boolean}
;; Does the board have an empty space at the given posn, and is the given posn adjacent to any
;; existing tile?
(define (posn-empty+has-adjacent? board posn)
  (define empty-tile-at? (negate tile-at))
  ((conjoin empty-tile-at? has-adjacent-tiles?)
   board posn))


#; {Board Posn -> Boolean}
;; For the given board, does this posn have any directly adjacent tiles?
;; ASSUME: the given posn has no tile at its location.
(define (has-adjacent-tiles? board posn)
  (define neighbor-posns          (posn-neighbors/dirs posn direction-names))
  (define tile-at^                (curry tile-at board))
  (define occupied-adjacent-tiles (filter-map tile-at^ neighbor-posns))

  (pair? occupied-adjacent-tiles))


#; {Board -> [Listof Posn]}
;; Produce a list of open positions neighboring the posns of existing tiles.
(define (open-posns board)
  (define tiles-map          (board-map board))
  (define placed-tile-posns  (hash-keys tiles-map))
  (define get-all-neighbors  (curryr posn-neighbors/dirs direction-names))
  (define not-existing-tile? (negate (curry tile-at board)))

  (define all-open-posns
    (~>> placed-tile-posns
         (map get-all-neighbors)
         flatten
         remove-duplicates
         (filter not-existing-tile?)))
  all-open-posns)


#; {Tile [Listof Tile] -> Boolean}
;; Does the given tile share the same color or (inclusive) same color as every tile in the given
;; list?
(define (valid-tile-sequence? tile tiles)
  ((disjoin tiles-equal-color? tiles-equal-shape?)
   (cons tile tiles)))


#; {Board Tile Posn -> Boolean}
;; Is the given position a valid Q position to place the tile on the given board?
;; A valid Q position is one where the left and right adjacent tiles, if any, share the same shape
;; or (inclusive) the same color, and the up and down adjacent tiles, if any, share the same shape
;; or (inclusive) same color.
(define (valid-placement? board tile target-posn)
  #; {[Listof Direction] -> [Listof Tile]}
  ;; Gets the existing tiles in the given directions.
  (define (adjacent-tiles dirs)
    (define tile-at^          (curry tile-at board))
    (define target-neighbors  (curry posn-neighbors/dirs target-posn))
    (filter-map tile-at^      (target-neighbors dirs)))

  (define row-adjacent-tiles (adjacent-tiles (hash-keys horizontal-directions)))
  (define col-adjacent-tiles (adjacent-tiles (hash-keys vertical-directions)))

  (define/lazy adjacent-tiles? (has-adjacent-tiles? board target-posn))
  (define/lazy matches-neighbors?
    (andmap (curry valid-tile-sequence? tile)
            (list row-adjacent-tiles col-adjacent-tiles)))

  (and adjacent-tiles? matches-neighbors?))



(module+ test
  (require rackunit)

  (define example-board
    (board (hash (posn 0 0) (tile 'red 'square)
                 (posn 1 0) (tile 'red 'circle)
                 (posn 0 1) (tile 'green 'square)
                 (posn 2 0) (tile 'red 'star)))))

(module+ test
  (test-equal?
   "create a board with the referees tile being a red square"
   (make-board (tile 'red 'square))
   (board++ #:map (hash (posn 0 0) (tile 'red 'square))))

  (test-equal?
   "create a board with the referees tile being a blue circle"
   (make-board (tile 'blue 'circle))
   (board++ #:map (hash (posn 0 0) (tile 'blue 'circle))))

  (test-equal?
   "board tile at (0, 0) with existing tile"
   (tile-at example-board (posn 0 0))
   (tile 'red 'square))

  (test-false
   "board tile at (-1, 0) without existing tile"
   (tile-at example-board (posn -1 0)))

  (test-true
   "board tile at (1, 1) has adjacent"
   (has-adjacent-tiles? example-board (posn 1 1)))

  (test-false
   "board tile at (-2, -2) does not have adjacent"
   (has-adjacent-tiles? example-board (posn -2 -2)))

  (test-equal?
   "posn adjacent dirs at (0, 0) with no directions given"
   (posn-neighbors/dirs (posn 0 0) '())
   '())

  (test-equal?
   "posn adjacent dirs at (0, 0) with every direction given"
   (posn-neighbors/dirs (posn 0 0) '(left right up down))
   (list (posn 0 -1) (posn 0 1) (posn -1 0) (posn 1 0)))

  (test-equal?
    "posn adjacent dirs at (1, 1) with some directions given"
    (posn-neighbors/dirs (posn 1 1) '(left up down))
    (list (posn 1 0) (posn 0 1) (posn 2 1)))

  (test-equal?
   "posn adjacent dirs at (1, 1) with duplicate directions given"
   (posn-neighbors/dirs (posn 1 1) '(left left))
   (list (posn 1 0) (posn 1 0)))

  (test-true
   "posn is empty and has adjacent tiles at (1, 1)"
   (posn-empty+has-adjacent? example-board (posn 1 1)))

  (test-false
   "posn is empty and has no adjacent tiles at (2, 2)"
   (posn-empty+has-adjacent? example-board (posn 2 2)))

  (test-false
   "posn is not empty and has adjacent tiles at (0, 0)"
   (posn-empty+has-adjacent? example-board (posn 0 0)))

  (test-equal?
   "add a tile to the board at (1, 1)"
   (board-map (add-tile example-board (posn 1 1) (tile 'blue 'square)))
   (hash (posn 0 0) (tile 'red 'square)
         (posn 1 0) (tile 'red 'circle)
         (posn 0 1) (tile 'green 'square)
         (posn 2 0) (tile 'red 'star)
         (posn 1 1) (tile 'blue 'square)))

  (test-exn
   "add a tile to the board at (0, 0) with an existing tile"
   exn:fail?
   (thunk (add-tile example-board (posn 0 0) (tile 'blue 'square))))

  (test-exn
   "add a tile to the board at (2, 2) with no adjacent tiles"
   exn:fail?
   (thunk (add-tile example-board (posn 2 2) (tile 'blue 'square))))

  (test-check
   "board open posns of example board"
   set=?
   (open-posns example-board)
   (list (posn 0 -1) (posn -1 0) (posn 3 0) (posn 2 -1) (posn 2 1) (posn 1 -1) (posn 1 1) (posn -1 1) (posn 0 2)))

  (test-check
   "board open posns of starting board"
   set=?
   (open-posns (make-board (tile 'red 'square)))
   (list (posn 1 0) (posn 0 -1) (posn -1 0) (posn 0 1)))

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
   (valid-placement? example-board (tile 'green 'circle) (posn 1 1)))

  (test-false
   "board invalid position for an invalid tile at (1, 1)"
   (valid-placement? example-board (tile 'green 'square) (posn 1 1)))

  (test-check
   "possible tile positions for a red clover"
   set=?
   (valid-tile-placements (tile 'red 'clover) example-board)
   (list (posn 0 -1) (posn -1 0) (posn 3 0) (posn 2 -1) (posn 2 1) (posn 1 -1)))

  (test-check
   "possible tile positions for a green square"
   set=?
   (valid-tile-placements (tile 'green 'square) example-board)
   (list (posn 0 -1) (posn -1 0) (posn -1 1) (posn 0 2)))

  (test-check
   "no possible tile positions for a blue 8star"
   set=?
   (valid-tile-placements (tile 'blue '8star) example-board)
   '()))
