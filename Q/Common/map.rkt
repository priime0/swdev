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

 board-can-place?
 board-empty-space?
 board-posn-has-adjacent?
 board-get-all-adjacent
 board-get-occupied-adjacent-posns
 board-add-tile
 (contract-out
  #:unprotected-submodule no-contract
  [make-board (-> tile? board?)]))

#; {type Board = (board [HashTable [Pairof Integer]
                                   Tile])}
;; A Board represents a square grid map in Q, implemented as a hash table where the keys are 2D
;; coordinates relative to the root tile, and the values are the tiles at the given coordinate.
;; INVARIANT: A Board is a connected graph.
;; INVARIANT: No other tile will be placed on an existing tile -- that is, two tiles cannot share
;;            the same key/coordinates, and a tile cannot replace a tile that is already on the map.
;; INVARIANT: The Tile placed at (0, 0) is the *root* tile of the game, which is the only referee
;;            placed-tile. All other tiles have coordinates that are relative to this tile.
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

#; {Board [Pairof Integer] -> Boolean}
;; Is the given posn valid? That is, does the board have an empty space at the given posn, and is
;; the given posn adjacent to any existing tile?
(define (board-can-place? board posn)
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
  (define relative-directions '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))
  (define neighboring-positions
    (for/list ([relative-direction relative-directions])
      (match-define (cons relative-row relative-col) relative-direction)
      (cons (+ row relative-row)
            (+ col relative-col))))
  neighboring-positions)

#; {Board [Pairof Integer] -> [Listof [Pairof Integer]]}
;; Get adjacent positions for the given posn where the adjacent positions are occupied by a tile.
(define (board-get-occupied-adjacent-posns board posn)
  (define map (board-map board))
  (define neighboring-positions (board-get-all-adjacent posn))
  (filter (curry hash-has-key? map)
          neighboring-positions))

#; {Board [Pairof Integer] Tile -> Board}
;; Places the new tile at the given posn on the board's map.
;; EXCEPT: Throws an error if the given position is invalid.
(define (board-add-tile board posn new-tile)
  (unless (board-can-place? board posn)
    (error 'board-add-tile
           "posn ~a invalid position to place on board"
           posn))

  (define existing-map (board-map board))
  (define new-map (hash-set existing-map posn new-tile))
  (set-board-map board new-map))

#; {Board Tile -> [Listof [Pairof Integer]]}
;; Produce a list of valid posns where the tile can be placed in the board.
(define (board-possible-tile-posns board tile)
  (define tiles-map (board-map board))
  (define insert-tile-shape (tile-shape tile))
  (define insert-tile-color (tile-color tile))
  (define existing-posns (hash-keys tiles-map))
  (define not-existing-tile? (curry (negate hash-has-key?) tiles-map))
  (define open-posns
    (~>> existing-posns
         (map board-get-all-adjacent _)
         (apply append)
         remove-duplicates
         (filter not-existing-tile?)))
  (define satisfying-tiles
    (for/list ([open-posn open-posns])
      (define adjacent-tile-posns (board-get-occupied-adjacent-posns board open-posn))
      (define adjacent-tiles (map (curry hash-ref tiles-map) adjacent-tile-posns))
      (define adjacent-tiles-shapes (remove-duplicates (map tile-shape adjacent-tiles)))
      (define adjacent-tiles-colors (remove-duplicates (map tile-color adjacent-tiles)))
      (match (cons adjacent-tiles-shapes adjacent-tiles-colors)
        [(or (cons (list (== insert-tile-shape))
                   (list (== insert-tile-color)))
             (cons (list (== insert-tile-shape)) _)
             (cons _ (list (== insert-tile-color))))
         (list open-posn)]
        [_ '()])))
  (remove-duplicates (apply append satisfying-tiles)))
