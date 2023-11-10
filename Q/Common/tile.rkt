#lang racket

(require struct-plus-plus)
(require threading)
(require 2htdp/image)

(require Q/Common/config)
(require Q/Lib/list)
(require Q/Lib/struct)
(require Q/Common/interfaces/serializable)

(provide
 tile
 tile?
 tile++
 tile-shape
 tile-color
 tiles-equal-color?
 tiles-equal-shape?
 hash->tile++
 empty-tile-image
 start-tiles
 tile-set
 (contract-out
  #:unprotected-submodule no-contract
  [tile-shapes (listof symbol?)]
  [tile-colors (listof symbol?)]
  [tile-shape? (-> any/c boolean?)]
  [tile-color? (-> any/c boolean?)]
  [tile<       (-> tile? tile? boolean?)]
  [sort-tiles  (-> (listof tile?) (listof tile?))]
  [render-tile (-> tile? image?)]
  [render-tiles (-> (listof tile?) image?)]))

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================


#; {type TileShape = (U 'star '8star 'square 'circle 'clover 'diamond)}
;; A TileShape is an enumeration of possible shapes, a distinguishing feature of a tile.
;; The ordering of TileShapes is from smallest to largest for the game of Q.
(define tile-shapes '(star 8star square circle clover diamond))


#; {type TileColor = (U 'red 'green 'blue 'yellow 'orange 'purple)}
;; A TileColor is an enumeration of possible colors, a distinguishing feature of a tile.
(define tile-colors '(red green blue yellow orange purple))

#; {Any -> Boolean}
;; Is the given item a TileShape?
(define (tile-shape? item)
  (member? item tile-shapes))


#; {TileShape TileShape -> Boolean}
;; Is the first tile shape less than the second lexicographically?
(define (shape< shape1 shape2)
  (< (index-of tile-shapes shape1)
     (index-of tile-shapes shape2)))


#; {Any -> Boolean}
;; Is the given item a TileColor?
(define (tile-color? item)
  (member? item tile-colors))


#; {TileColor TileColor -> Boolean}
;; Is the first tile color less than the second, lexicographically?
(define (color< color1 color2)
  (< (index-of tile-colors color1)
     (index-of tile-colors color2)))


#; {Tile Tile -> Boolean}
;; Is the first tile less than the second, lexicographically?
(define (tile< tile1 tile2)
  (match-define [tile color1 shape1] tile1)
  (match-define [tile color2 shape2] tile2)
  (or (shape< shape1 shape2)
      (and (eq? shape1 shape2)
           (color< color1 color2))))


#; {type Tile = (tile TileShape TileColor)}
;; A Tile is a distinguishable object by shapes and colors, placed and held by players in the Q
;; board game.
(struct++ tile
          ([color tile-color?]
           [shape tile-shape?])
          (#:convert-from
           (hash
            [hash?
             (hash-table ('color (app string->symbol color))
                         ('shape (app string->symbol shape)))
             (color shape)]))
          #:transparent
          #:methods gen:serializable
          [(define (->jsexpr t)
             (match-define [tile color shape] t)
             (hash 'color (symbol->string color)
                   'shape (symbol->string shape)))])


;; ========================================================================================
;; CORE FUNCTIONALITY
;; ========================================================================================

;; The set of all possible tiles from the possible shapes and colors.
(define tile-set (map (curry apply tile) (cartesian-product tile-colors tile-shapes)))
(define num-tile-sets 30)

;; The default 1080 starting tiles for a game of Q.
(define start-tiles (flatten (build-list num-tile-sets (thunk* tile-set))))


#; {[Listof Tile] -> [Listof Tile]}
;; Sorts the tiles in terms of ascending lexicographic ordering of
;; shapes then colors based on the definition of tile-shapes and
;; tile-colors, which represent the canonical low->high ordering of
;; tiles.
(define (sort-tiles tiles)
  ((sort-by (list (cons (curry index<? tile-shapes) tile-shape)
                  (cons (curry index<? tile-colors) tile-color)))
   tiles))


#; {[Listof Tile] -> Boolean}
;; Do all the tiles have equal colors?
(define (tiles-equal-color? tiles)
  (struct-equal-field? tile-color tiles))


#; {[Listof Tile] -> Boolean}
;; Do all the tiles have equal shapes?
(define (tiles-equal-shape? tiles)
  (struct-equal-field? tile-shape tiles))


;; ========================================================================================
;; RENDERING FUNCTIONALITY
;; ========================================================================================


#; {Tile -> Image}
;; Produce an image of the tile, clearly displaying its shape and color. Normalizes images produced
;; by helper functions to be the same width.
(define (render-tile tile)
  (define shape-function (~> tile tile-shape render-tile/shape-function))
  (define color (tile-color tile))
  (frame (render-tile/normalize (shape-function color))))


#; {[Listof Tile] -> Image}
;; Produce an image of the given list of tiles, concatenated together.
(define (render-tiles tiles #:combine-fn [combine beside])
  (cond [(null? tiles)       empty-image]
        [(null? (cdr tiles)) (render-tile (car tiles))]
        [else                (foldr combine empty-image (map render-tile tiles))]))


;; An empty tile image used for rendering posns in the board with no tiles on them.
(define empty-tile-image (square (*game-size*) 'solid (*background-color*)))


#; {Natural TileColor -> Image}
;; Produces a function that consumes a color and generates an image of a star with the given
;; `point-count`, the number of points the star has.
(define (render-tile/nstar point-count color)
  (define inner-radius (/ (*game-size*) 4))
  (define outer-radius (*game-size*))
  (radial-star point-count inner-radius outer-radius (*tile-shape-mode*) color))


#; {TileColor -> Image}
;; Produces an image of a 4-side star with the given color.
(define (render-tile/star color)
  (define point-count 4)
  (render-tile/nstar point-count color))


#; {TileColor -> Image}
;; Produces an image of a 8-side star with the given color.
(define (render-tile/8star color)
  (define point-count 8)
  (render-tile/nstar point-count color))


#; {TileColor -> Image}
;; Produces an image of a square with the given color.
(define (render-tile/square color)
  (define side-length (*game-size*))
  (square side-length (*tile-shape-mode*) color))


#; {TileColor -> Image}
;; Produces an image of a circle with the given color.
(define (render-tile/circle color)
  (define radius (*game-size*))
  (circle radius (*tile-shape-mode*) color))


#; {TileColor -> Image}
;; Produces an image of a circle with the given color.
(define (render-tile/diamond color)
  (define side-length (*game-size*))
  (define angle 90)
  (rhombus side-length angle (*tile-shape-mode*) color))


#; {TileColor -> Image}
;; Produces an image of a clover with the given color.
(define (render-tile/clover color)
  (define major-radius (*game-size*))
  (define minor-radius (/ (*game-size*) 2))
  (overlay (ellipse major-radius minor-radius 'solid color)
           (ellipse minor-radius major-radius 'solid color)))


#; {Image -> Image}
;; Normalizes the size of a tile image to be the defined *game-size*. Images of either zero width or
;; zero height maintain the same unaltered width or height, respectively.
(define (render-tile/normalize tile-image)
  (define current-width (image-width tile-image))
  (define target-width (*game-size*))
  (cond [(zero? current-width) tile-image]
        [else
         (define scale-factor (/ target-width current-width))
         (scale scale-factor tile-image)]))


#; {TileShape -> TileColor -> Image}
;; Given a TileShape, produce a function that takes in a tile color and renders an image with the
;; given shape and color.
(define (render-tile/shape-function tile-shape)
  (match tile-shape
    ['star    render-tile/star]
    ['8star   render-tile/8star]
    ['square  render-tile/square]
    ['circle  render-tile/circle]
    ['diamond render-tile/diamond]
    ['clover  render-tile/clover]))


;; ========================================================================================
;; UNIT TESTS
;; ========================================================================================


(module+ test
  (define red-square-tile (tile 'red 'square))
  (define red-star-tile (tile 'red 'star))
  (define blue-star-tile (tile 'blue 'star))
  (define blue-square-tile (tile 'blue 'square)))

(module+ test
  (require rackunit))

(module+ test
  (test-equal?
   "normalize empty image into empty image"
   (render-tile/normalize empty-image)
   empty-image)

  (test-equal?
   "normalize big blue star into *game-size* width"
   (image-width (render-tile/normalize (radial-star 4 80 360 'solid 'blue)))
   (*game-size*))

  (test-true
   "different tiles normalized are the same width"
   (equal? (image-width (render-tile/normalize (radial-star 4 80 360 'solid 'blue)))
           (image-width (render-tile/normalize (circle 95 'solid 'red))))))

(module+ test
  (parameterize ([*game-size* 100])
    (test-equal?
     "render red square correctly"
     (render-tile red-square-tile)
     (square 100 'solid 'red))

    (test-equal?
     "render blue star correctly"
     (render-tile blue-star-tile)
     (scale 5/7 (radial-star 4 25 100 'solid 'blue)))))

(module+ test
  (test-true
   "equal color of tiles"
   (tiles-equal-color? (list red-square-tile red-star-tile)))

  (test-false
   "unequal color of tiles"
   (tiles-equal-color? (list red-square-tile blue-star-tile)))

  (test-true
   "equal shape of tiles"
   (tiles-equal-shape? (list red-square-tile blue-square-tile)))

  (test-false
   "unequal shape of tiles"
   (tiles-equal-shape? (list red-square-tile blue-star-tile))))
