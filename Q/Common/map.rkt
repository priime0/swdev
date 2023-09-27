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

#; {type Board = (board [HashTable [Pairof Integer]
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

#; {type Direction = (U 'up 'down 'left 'right)}
;; A Direction represents a pair of (δr . δc), i.e. a translation in one of the four directions
;; in a square grid.
(define directions
  #hash([up    . (-1 . 0)]
        [down  . (1 . 0)]
        [left  . (0 . -1)]
        [right . (0 . 1)]))

#; {[Pairof Integer] Direction -> [Pairof Integer]}
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

#; {Board [Pairof Integer] -> [Maybe Tile]}
;; Gets the tile at `posn` if it exists, otherwise #f
(define (board-tile-at board posn)
  (define tiles-map (board-map board))
  ((conjoin hash-has-key? hash-ref) 
   tiles-map posn))

#; {Board [Pairof Integer] -> Boolean}
;; Is the given posn valid? That is, does the board have an empty space at the given posn, and is
;; the given posn adjacent to any existing tile?
(define (board-posn-empty+has-adjacent? board posn)
  ((conjoin board-empty-space? board-posn-has-adjacent?)
   board posn))

#; {Board [Pairof Integer] -> Boolean}
;; Does the board have an empty space at the given posn?
(define (board-empty-space? board posn)
  (define map (board-map board))
  (not (hash-has-key? map posn)))

#; {Board [Pairof Integer] -> Boolean}
;; Is the given posn adjacent to any existing tile?
;; ASSUME: the given posn has no tile at its location.
(define (board-posn-has-adjacent? board posn)
  (pair? (board-get-occupied-adjacent-posns board posn)))

#; {[Pairof Integer] -> [Listof [Pairof Integer]]}
;; Get all numerically adjacent positions for the given posn.
(define (board-get-all-adjacent posn)
  (match-define (cons row col) posn)
  (define neighboring-positions
    (for/list ([relative-direction (hash-values directions)])
      (match-define (cons relative-row relative-col) relative-direction)
      (cons (+ row relative-row)
            (+ col relative-col))))
  neighboring-positions)

#; {Board [Pairof Integer] -> [Listof [Pairof Integer]]}
;; Get tile-occupied adjacent positions for the given posn.
;; ASSUME `(board-can-place? board posn)`
(define (board-get-occupied-adjacent-posns board posn)
  (define map (board-map board))
  (define neighboring-positions (board-get-all-adjacent posn))
  (filter (curry hash-has-key? map)
          neighboring-positions))

#; {Board [Pairof Integer] Tile -> Board}
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

#; {Board -> [Listof [Pairof Integer]]}
;; Produce a list of open positions adjacent to existing tiles.
(define (board-open-posns board)
  (define tiles-map          (board-map board))
  (define placed-tile-posns  (hash-keys tiles-map))
  (define not-existing-tile? (curry (negate hash-has-key?) tiles-map))
  (define open-posns
    (~>> placed-tile-posns
         (map board-get-all-adjacent)
         (apply append)
         remove-duplicates
         (filter not-existing-tile?)))
  open-posns)

#; {[Listof Tile] -> Boolean}
;; Do the list of tiles share either the same color or (inclusive) same shape?
(define (valid-tile-sequence? tiles)
  (or (apply tiles-equal-color? tiles)
      (apply tiles-equal-shape? tiles)))



  
#; {Board Tile [Pairof Integer] -> [Maybe [Pairof Integer]]}
;; Returns the given `target-posn` if it's a valid position to place the tile, otherwise `#f`.
;; A valid Q position is one where, for a contiguous row/column (including the candidate posn), all
;; tiles in the contiguous row/column share either the same shape or (inclusive) same color.
(define (board-valid-posn board tile target-posn)
  (define (tile-in-dir dir)
    (board-tile-at board (posn-translate posn dir)))
  (define horizontal-sequence
    (filter identity (list tile (tile-in-dir 'left) (tile-in-dir 'right))))
  (define vertical-sequence
    (filter identity (list tile (tile-in-dir 'up) (tile-in-dir 'down))))
  (define valid-placement? (and (valid-tile-sequence? horizontal-sequence)
                                (valid-tile-sequence? vertical-sequence)))

  (and valid-placement? target-posn))

#; {Board Tile -> [Listof [Pairof Integer]]}
;; Produce a list of valid posns where the tile can be placed in the board.
(define (board-possible-tile-posns board tile)
  (define open-posns (board-open-posns board))
  (filter-map (curry board-valid-posn board tile) open-posns))
