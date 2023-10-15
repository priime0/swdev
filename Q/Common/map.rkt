#lang racket

(require racket/generic)

(require (rename-in (only-in lazy define)
                    [define define/lazy]))

(require (rename-in data/functor
                    [map fmap]))

(require struct-plus-plus)
(require threading)
(require 2htdp/image)

(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/data/turn-action)
(require Q/Common/util/misc)
(require Q/Common/interfaces/serializable)

(provide
 (rename-out [valid-board? board?])
 sequence?
 board-map
 bounds
 place-tiles
 tile-at
 (contract-out
  [make-board (-> tile? valid-board?)]
  [add-tile
   (->i ([b valid-board?] [p posn?] [t tile?])
        #:pre/name (b p)
        "given posn isn't empty"
        ((negate tile-at) b p)
        #:pre/name (b p)
        "given posn has no adjacent tiles"
        (has-adjacent-tiles? b p)
        [result valid-board?])]
  [valid-placement?
   (-> valid-board?
       tile?
       posn?
       boolean?)]
  [valid-tile-placements
   (-> tile?
       valid-board?
       (listof posn?))]
  [collect-sequence
   (-> valid-board?
       posn?
       axis?
       sequence?)]
  [hash->board++
   (-> (listof (cons/c integer? (listof (cons/c integer? any/c))))
       valid-board?)]
  [render-board
   (-> valid-board? image?)]))


#; {type Board = (board [HashTable Posn Tile])}
;; A Board represents a map, implemented as a hash table where the keys are 2D coordinates relative
;; to the root tile, and the values are the tiles at the given coordinate.
;; DEFINITION: Two tiles are considered _adjacent_ IFF WLOG the posns where the tiles are placed are
;;             neighbors.
;; INVARIANT 0: A Board is a connected graph.
;; INVARIANT 1: No other tile will be placed on an existing tile -- that is, two tiles cannot share
;;              the same key/coordinates, and a tile cannot replace a tile that is already on the
;;              map.
;; INVARIANT 2: A position on the board is occupied by a tile IFF the position is a key in the hash
;;              map.
(struct++ board
          ([map (hash/c posn? tile?)])
          #:transparent
          #:methods gen:functor
          [(define (map f x)
             (define b (board (f (board-map x))))
             (unless (valid-board? b)
               (error 'fmap "fmap produced an invalid board"))
             b)]
          #:methods gen:serializable
          [(define/generic ->jsexpr* ->jsexpr)
           (define (->jsexpr b)
             (define h (make-hash))
             (for ([(p t) (in-hash (board-map b))])
               (match-define [posn row column] p)
               (define t^ (->jsexpr* t))
               (hash-set! h row (cons (list column t^)
                                      (hash-ref h row '()))))
             (for/list ([(row cells) (in-hash h)])
               (cons row cells)))])


;; {type Sequence = [Listof TilePlacement]}
;; A Sequence is a a collection of distinct posn-tile pairs
;; such that for any two different posn-tile pairs α, β in the sequence,
;; reachable?(α, β).

;; We define two distinct posn-tile pairs α, β to be _reachable_ IFF WLOG
;; they share one axis and for all posns φ between the posns of α and β,
;; φ is occupied by a tile.
(define (sequence? a)
  (and (list? a)
       (andmap placement? a)
       (same-axis? (map placement-posn a))))


#; {Any -> Boolean}
(define (valid-board? a)
  (let/ec return
    (unless (board? a)
      (return #f))
    
    (define bmap (board-map a))
    (define root-posn (posn 0 0))
    (define initial-state?
      (and (one? (hash-count bmap))
           (hash-has-key? bmap root-posn)
           (tile? (hash-ref bmap root-posn))))
    
    (or initial-state?
        (and (positive? (hash-count bmap))
             (andmap posn? (hash-keys bmap))
             (andmap tile? (hash-values bmap))
             (andmap (curry has-adjacent-tiles? a) (hash-keys bmap))))))


#; {JMap -> Board}
(define (hash->board++ rows)
  (define h (make-hash))
  (for* ([row rows] [cell (rest row)])
    (define r (first row))
    (match-define [list c jtile] cell)
    (define p (posn r c))
    (define t (hash->tile++ jtile))
    (hash-set! h p t))
  
  (board (make-immutable-hash (hash->list h))))


#; {Tile -> Board}
;; Create a Board with the given Tile placed at position (0, 0) -- the Tile represents the *root*
;; tile of a game, which is the only referee-placed tile.
(define (make-board root-tile)
  (define root-posn (posn 0 0))
  (board++ #:map (hash root-posn root-tile)))


#; {Board [Listof TilePlacement] -> Board}
;; Constructs a new board with all of the given tile placements.
;; ASSUME each tile placement is valid.
(define (place-tiles board placements)
  (for/fold ([board^ board])
            ([pment placements])
    (match-define [placement posn tile] pment)
    (add-tile board^ posn tile)))


#; {Board Posn Tile -> Board}
;; Places the new tile at the given posn on the board's map.
(define (add-tile board posn new-tile)
  (define add-tile+ (curryr hash-set posn new-tile))
  (fmap add-tile+ board))


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
  (define tile-at^           (curry tile-at board))
  
  (define all-open-posns
    (~>> placed-tile-posns
         (map get-all-neighbors)
         flatten
         remove-duplicates
         (filter-not tile-at^)))
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
  
  (define row-adjacent-tiles (adjacent-tiles horizontal-axis))
  (define col-adjacent-tiles (adjacent-tiles vertical-axis))
  
  (define/lazy adjacent-tiles? (has-adjacent-tiles? board target-posn))
  (define/lazy matches-neighbors?
    (andmap (curry valid-tile-sequence? tile)
            (list row-adjacent-tiles col-adjacent-tiles)))
  
  (and adjacent-tiles? matches-neighbors?))


#; {Board [Pairof Integer] Direction -> Sequence}
;; Collect the contiguous sequence of tiles towards the given direction starting from but not
;; including the given position.
(define (collect-sequence/dir board posn dir)
  (define start-posn (posn-translate posn dir))
  ;; generative: produce a list of tiles along the contiguous sequence in reverse order
  ;; terminates: when we find a posn along the direction unoccupied by a tile, since there exists a
  ;;             finite amount of tiles on the board at any moment.
  (define seq
    (let loop ([current-posn start-posn]
               [seq '()])
      (cond
        [(tile-at board current-posn)
         => (lambda (t)
              (define posn+ (posn-translate current-posn dir))
              (define seq+ (cons (placement current-posn t) seq))
              (loop posn+ seq+))]
        [else seq])))
  
  (reverse seq))

#; {Board Posn Axis -> Sequence}
;; Collect the sequence of tiles that runs along the given axis,
;; rooted at the tile at the given position, or return empty list if
;; there is no such tile
(define (collect-sequence board posn axis)
  (cond
    [(tile-at board posn)
     => (lambda (t)
          (define dir1-seq (collect-sequence/dir board posn (car axis)))
          (define dir2-seq (collect-sequence/dir board posn (cadr axis)))
          (append (reverse dir1-seq)
                  (list (placement posn t))
                  dir2-seq))]
    [else '()]))


#; {Board -> (values Integer Integer Integer Integer)}
;; Retrieve the top, bottom, left, and right bounds (inclusive) of the board.
(define (bounds b)
  (define bmap (board-map b))
  (for/fold ([top 0] [bot 0] [left 0] [right 0])
            ([(p _) (in-hash bmap)])
    (match-define [posn r c] p)
    (values (min top r)
            (max bot r)
            (min left c)
            (max right c))))

#; {Board -> Image}
(define (render-board b)
  (define-values (top-bound bot-bound left-bound right-bound)
    (bounds b))

  (for/fold ([image empty-image])
            ([r (in-inclusive-range top-bound bot-bound)])
    (above image
           (for/fold ([row-image empty-image])
                     ([c (in-inclusive-range left-bound right-bound)])
             (define t (tile-at b (posn r c)))
             (beside row-image
                     (if t (render-tile t) empty-tile-image))))))


(module+ test
  (require rackunit)
  (require Q/Common/util/test)

  (define example-board
    (board (hash (posn 0 0) (tile 'red 'square)
                 (posn 1 0) (tile 'red 'circle)
                 (posn 0 1) (tile 'green 'square)
                 (posn 2 0) (tile 'red 'star))))
  (define example-board2
    (board (hash (posn 0 0)   (tile 'red 'square)
                 (posn 0 -1)  (tile 'green 'circle)
                 (posn -1 -1) (tile 'green 'square)
                 (posn 1 0)   (tile 'red 'star)
                 (posn 2 0)   (tile 'blue 'star)
                 (posn 2 1)   (tile 'blue '8star)))))

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
   '())
  
  (test-values-equal?
   "bounds of example board"
   (bounds example-board)
   (values 0 2 0 1))

  (test-values-equal?
   "bounds of starter board"
   (bounds (make-board (tile 'red 'square)))
   (values 0 0 0 0))

  (test-values-equal?
   "bounds of a elongated board"
   (bounds example-board2)
   (values -1 2 -1 1)))
