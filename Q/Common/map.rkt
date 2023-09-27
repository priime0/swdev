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

#; {type Posn = Posn}
;; A Posn is a pair of '(r . c), which represent a row and column in the Board.

#; {type Direction = (U 'up 'down 'left 'right)}
;; A Direction represents a pair of '(δr . δc), i.e. a translation in one of the four directions
;; in a square grid.
(define directions
  #hash([up    . (-1 . 0)]
        [down  . (1 . 0)]
        [left  . (0 . -1)]
        [right . (0 . 1)]))

#; {Posn Direction -> Posn}
;; Produce a new posn representing the given posn translated towards the given direction.
(define (posn-translate posn dir)
  (match-define (cons base-r base-c) posn)
  (match-define (cons dr dc) (hash-ref directions dir))
  (cons (+ base-r dr)
        (+ base-c dc)))

#; {Tile -> Board}
;; Create a Board with the given Tile placed at position (0, 0) -- the Tile represents the *root*
;; tile of a game, which is the only referee-placed tile.
(define (make-board root-tile)
  (board++ #:map (hash '(0 . 0) root-tile)))

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
  ((conjoin (negate board-tile-at) board-posn-has-adjacent?)
   board posn))

#; {Board Posn -> Boolean}
;; Is the given posn adjacent to any existing tile?
;; ASSUME: the given posn has no tile at its location.
(define (board-posn-has-adjacent? board posn)
  (pair? (board-get-occupied-adjacent-posns board posn)))

#; {Posn [Listof Direction] -> [Listof Posn]}
;; Produces the list of adjacent positions for the given posn for each direction in the given list.
(define (posn-adjacent/dirs posn dir-list)
  (define posn-translate+ (curry posn-translate posn))
  (map posn-translate+ dir-list))
                      
#; {Board Posn -> [Listof Posn]}
;; Get tile-occupied adjacent positions for the given posn.
;; ASSUME `(board-can-place? board posn)`
(define (board-get-occupied-adjacent-posns board posn)
  (define adjacent-posns (posn-adjacent/dirs (hash-keys directions) posn))
  (define map (board-map board))
  (filter (curry hash-has-key? map) adjacent-posns))

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

#; {[Listof Tile] -> Boolean}
;; Do the lits of tiles share either the same color or (inclusive) same shape?
(define (valid-tile-sequence? tiles)
  ((disjoin tiles-equal-color? tiles-equal-shape?)
   tiles))

#; {Board Tile Posn -> [Maybe Posn]}
;; Returns the given `target-posn` if it's a valid position to place the tile, otherwise `#f`.
;; A valid Q position is one where the left and right neighbors share the same shape or (inclusive)
;; the same color, and the up and down neighbors share the same shape or (inclusive) same color.
(define (board-valid-posn board tile target-posn)
  (define tile-at                   (curry board-tile-at board))
  (define target-posn-adjacent/dirs (curry posn-adjacent/dirs target-posn))
  (define row-neighbor-posns        (target-posn-adjacent/dirs '(left right)))
  (define col-neighbor-posns        (target-posn-adjacent/dirs '(up down)))
  (define row-neighbor-tiles        (cons tile (filter-map tile-at row-neighbor-posns)))
  (define col-neighbor-tiles        (cons tile (filter-map tile-at col-neighbor-posns)))

  (define valid-placement?
    (andmap valid-tile-sequence? (list row-neighbor-tiles col-neighbor-tiles)))

  (and valid-placement? target-posn))

#; {Board Tile -> [Listof Posn]}
;; Produce a list of valid posns where the tile can be placed in the board.
(define (board-possible-tile-posns board tile)
  (define open-posns (board-open-posns board))
  (filter-map (curry board-valid-posn board tile) open-posns))
