#lang racket

(require struct-plus-plus)
(require threading)

(require Q/Common/data/tile)

(provide
 board
 board?
 board++
 board-map
 set-board-map

 (contract-out
  #:unprotected-submodule no-contract
  [make-board (-> tile? board?)]
  [board-add-tile
   (-> board?
       (cons/c integer? integer?)
       tile?
       board?)]
  [board-possible-tile-posns
   (-> board?
       tile?
       (listof (cons/c integer? integer?)))]))


#; {type Posn = Posn}
;; A Posn is a pair of '(r . c), which represent a row and column in the Board.

#; {Posn Direction -> Posn}
;; Produce a new posn representing the given posn translated towards the given direction.
(define (posn-translate posn dir)
  (match-define (cons base-r base-c) posn)
  (match-define (cons dr dc) (hash-ref directions dir))
  (cons (+ base-r dr)
        (+ base-c dc)))


#; {type Direction = (U 'up 'down 'left 'right)}
;; A Direction represents a pair of '(δr . δc), i.e. a translation in one of the four directions
;; in a square grid.
(define directions
  #hash([up    . (-1 . 0)]
        [down  . (1 . 0)]
        [left  . (0 . -1)]
        [right . (0 . 1)]))


#; {type Board = (board [HashTable Posn
                                   Tile])}
;; A Board represents a square grid map in Q, implemented as a hash table where the keys are 2D
;; coordinates relative to the root tile, and the values are the tiles at the given coordinate.
;; INVARIANT: A Board is a connected graph.
;; INVARIANT: The Tile placed at (0, 0) is the *root* tile of the game, which is the only referee
;;            placed-tile. All other tiles have coordinates that are relative to this tile.
;; INVARIANT: No other tile will be placed on an existing tile -- that is, two tiles cannot share
;;            the same key/coordinates, and a tile cannot replace a tile that is already on the map.
;; INVARIANT: A position on the board is occupied by a tile IFF the position is a key in the hash
;;            map.
(struct++ board
          ([map hash?])
          #:transparent)


#; {Tile -> Board}
;; Create a Board with the given Tile placed at position (0, 0) -- the Tile represents the *root*
;; tile of a game, which is the only referee-placed tile.
(define (make-board root-tile)
  (board++ #:map (hash '(0 . 0) root-tile)))


#; {Board Posn Tile -> Board}
;; Places the new tile at the given posn on the board's map.
;; EXCEPT: Throws an error if the given position is invalid.
(define (board-add-tile board posn new-tile)
  (unless (board-posn-empty+has-adjacent? board posn)
    (error 'board-add-tile
           "posn ~a invalid position to place on board"
           posn))

  (define existing-map (board-map board))
  (define new-map (hash-set existing-map posn new-tile))
  (set-board-map board new-map))


#; {Board Tile -> [Listof Posn]}
;; Produce a list of valid posns where the tile can be placed in the board.
(define (board-possible-tile-posns board tile)
  (define open-posns (board-open-posns board))
  (define valid-maybe-posn (curry board-valid-posn board tile))
  (filter-map valid-maybe-posn open-posns))


#; {Board Posn -> [Maybe Tile]}
;; Gets the tile at `posn` if it exists, otherwise #f
(define (board-tile-at board posn)
  (define tiles-map (board-map board))
  ((conjoin hash-has-key? hash-ref) 
   tiles-map posn))


#; {Board Posn -> Boolean}
;; Is the given posn valid? That is, does the board have an empty space at the given posn, and is
;; the given posn adjacent to any existing tile?
(define (board-posn-empty+has-adjacent? board posn)
  (define empty-tile-at? (negate board-tile-at))
  ((conjoin empty-tile-at? board-posn-has-adjacent?)
   board posn))


#; {Board Posn -> Boolean}
;; Is the given posn adjacent to any existing tile?
;; ASSUME: the given posn has no tile at its location.
(define (board-posn-has-adjacent? board posn)
  (define adjacent-posns (posn-adjacent/dirs posn (hash-keys directions)))
  (define tile-at (curry board-tile-at board))
  (define occupied-adjacent-posns (filter-map tile-at adjacent-posns))
  (pair? occupied-adjacent-posns))


#; {Posn [Listof Direction] -> [Listof Posn]}
;; Produces the list of adjacent positions for the given posn for each direction in the given list.
;; Does not filter out duplicate positions.
(define (posn-adjacent/dirs posn dir-list)
  (define posn-translate^ (curry posn-translate posn))
  (map posn-translate^ dir-list))


#; {Board -> [Listof Posn]}
;; Produce a list of open positions adjacent to existing tiles.
(define (board-open-posns board)
  (define tiles-map          (board-map board))
  (define placed-tile-posns  (hash-keys tiles-map))
  (define not-existing-tile? (curry (negate board-tile-at) board))
  (define get-all-adjacent   (curryr posn-adjacent/dirs (hash-keys directions)))
  (define open-posns
    (~>> placed-tile-posns
         (map get-all-adjacent)
         (apply append)
         remove-duplicates
         (filter not-existing-tile?)))
  open-posns)


#; {Tile [Listof Tile] -> Boolean}
;; Does the given tile share the same color or (inclusive) same color as every tile in the given
;; list?
(define (valid-tile-sequence? tile tiles)
  ((disjoin tiles-equal-color? tiles-equal-shape?)
   (cons tile tiles)))


#; {Board Tile Posn -> [Maybe Posn]}
;; Returns the given `target-posn` if it's a valid position to place the tile, otherwise `#f`.
;; A valid Q position is one where the left and right neighbors share the same shape or (inclusive)
;; the same color, and the up and down neighbors share the same shape or (inclusive) same color.
(define (board-valid-posn board tile target-posn)
  (define tile-at                   (curry board-tile-at board))
  (define target-posn-adjacent/dirs (curry posn-adjacent/dirs target-posn))
  (define row-neighbor-posns        (target-posn-adjacent/dirs '(left right)))
  (define col-neighbor-posns        (target-posn-adjacent/dirs '(up down)))
  (define row-neighbor-tiles        (filter-map tile-at row-neighbor-posns))
  (define col-neighbor-tiles        (filter-map tile-at col-neighbor-posns))

  (define valid-placement?
    (andmap (curry valid-tile-sequence? tile)
            (list row-neighbor-tiles col-neighbor-tiles)))

  (and valid-placement? target-posn))



(module+ test
  (require rackunit)

  (define example-board
    (board (hash '(0 . 0) (tile 'red 'square)
                 '(1 . 0) (tile 'red 'circle)
                 '(0 . 1) (tile 'green 'square)
                 '(2 . 0) (tile 'red 'star)))))

(module+ test
  (test-equal?
   "create a board with the referees tile being a red square"
   (make-board (tile 'red 'square))
   (board++ #:map (hash '(0 . 0) (tile 'red 'square))))

  (test-equal?
   "create a board with the referees tile being a blue circle"
   (make-board (tile 'blue 'circle))
   (board++ #:map (hash '(0 . 0) (tile 'blue 'circle))))

  (test-equal?
   "board tile at (0, 0) with existing tile"
   (board-tile-at example-board '(0 . 0))
   (tile 'red 'square))

  (test-false
   "board tile at (-1, 0) without existing tile"
   (board-tile-at example-board '(-1 . 0)))

  (test-true
   "board tile at (1, 1) has adjacent"
   (board-posn-has-adjacent? example-board '(1 . 1)))

  (test-false
   "board tile at (-2, -2) does not have adjacent"
   (board-posn-has-adjacent? example-board '(-2 . -2)))

  (test-equal?
   "posn adjacent dirs at (0, 0) with no directions given"
   (posn-adjacent/dirs '(0 . 0) '())
   '())

  (test-equal?
   "posn adjacent dirs at (0, 0) with every direction given"
   (posn-adjacent/dirs '(0 . 0) '(left right up down))
   '((0 . -1) (0 . 1) (-1 . 0) (1 . 0)))

  (test-equal?
    "posn adjacent dirs at (1, 1) with some directions given"
    (posn-adjacent/dirs '(1 . 1) '(left up down))
    '((1 . 0) (0 . 1) (2 . 1)))

  (test-equal?
   "posn adjacent dirs at (1, 1) with duplicate directions given"
   (posn-adjacent/dirs '(1 . 1) '(left left))
   '((1 . 0) (1 . 0)))

  (test-true
   "posn is empty and has adjacent tiles at (1, 1)"
   (board-posn-empty+has-adjacent? example-board '(1 . 1)))

  (test-false
   "posn is empty and has no adjacent tiles at (2, 2)"
   (board-posn-empty+has-adjacent? example-board '(2 . 2)))

  (test-false
   "posn is not empty and has adjacent tiles at (0, 0)"
   (board-posn-empty+has-adjacent? example-board '(0 . 0)))

  (test-equal?
   "add a tile to the board at (1, 1)"
   (board-map (board-add-tile example-board '(1 . 1) (tile 'blue 'square)))
   (hash '(0 . 0) (tile 'red 'square)
         '(1 . 0) (tile 'red 'circle)
         '(0 . 1) (tile 'green 'square)
         '(2 . 0) (tile 'red 'star)
         '(1 . 1) (tile 'blue 'square)))

  (test-exn
   "add a tile to the board at (0, 0) with an existing tile"
   exn:fail?
   (thunk (board-add-tile example-board '(0 . 0) (tile 'blue 'square))))

  (test-exn
   "add a tile to the board at (2, 2) with no adjacent tiles"
   exn:fail?
   (thunk (board-add-tile example-board '(2 . 2) (tile 'blue 'square))))

  (test-check
   "board open posns of example board"
   set=?
   (board-open-posns example-board)
   '((0 . -1) (-1 . 0) (3 . 0) (2 . -1) (2 . 1) (1 . -1) (1 . 1) (-1 . 1) (0 . 2)))

  (test-check
   "board open posns of starting board"
   set=?
   (board-open-posns (make-board (tile 'red 'square)))
   '((1 . 0) (0 . -1) (-1 . 0) (0 . 1)))

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

  (test-equal?
   "board valid position for a valid tile at (1, 1)"
   (board-valid-posn example-board (tile 'green 'circle) '(1 . 1))
   '(1 . 1))

  (test-false
   "board invalid position for an invalid tile at (1, 1)"
   (board-valid-posn example-board (tile 'green 'square) '(1 . 1)))

  (test-check
   "possible tile positions for a red clover"
   set=?
   (board-possible-tile-posns example-board (tile 'red 'clover))
   '((0 . -1) (-1 . 0) (3 . 0) (2 . -1) (2 . 1) (1 . -1)))

  (test-check
   "possible tile positions for a green square"
   set=?
   (board-possible-tile-posns example-board (tile 'green 'square))
   '((0 . -1) (-1 . 0) (-1 . 1) (0 . 2)))

  (test-check
   "no possible tile positions for a blue 8star"
   set=?
   (board-possible-tile-posns example-board (tile 'blue '8star))
   '()))
