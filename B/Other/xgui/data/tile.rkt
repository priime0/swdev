#lang racket

(require struct-plus-plus)
(require threading)
(require 2htdp/image)

(require "../config.rkt")
(require "../util/list.rkt")

(provide
 tile
 tile++
 tile-shape
 tile-color
 set-tile-shape
 set-tile-color
 (contract-out
  #:unprotected-submodule no-contract
  [tile-shapes (listof symbol?)]
  [tile-colors (listof symbol?)]
  [tile-shape? (any/c . -> . boolean?)]
  [tile-color? (any/c . -> . boolean?)]

  [render-tile (tile? . -> . image?)]))

#; {type TileShape = (U 'star '8star 'square 'circle 'diamond)}
;; A TileShape is an enumeration of possible shapes, a distinguishing feature of a tile.
(define tile-shapes '(star 8star square circle diamond))

#; {type TileColor = (U 'red 'green 'blue 'yellow 'orange 'purple)}
;; A TileColor is an enumeration of possible colors, a distinguishing feature of a tile.
(define tile-colors '(red green blue yellow orange purple))

#; {Any -> Boolean}
;; Is the given item a TileShape?
(define (tile-shape? item)
  (member? item tile-shapes))

#; {Any -> Boolean}
;; Is the given item a TileColor?
(define (tile-color? item)
  (member? item tile-colors))

#; {type Tile = (tile TileShape TileColor)}
;; A Tile is a distinguishable object by shapes and colors, placed and held by players in the Q
;; board game.
(struct++ tile
          ([shape tile-shape?]
           [color tile-color?])
          #:transparent)

;; Whether the shape is filled in ('solid) or not ('outline)
(define tile-shape-mode 'solid)

#; {Natural Color -> Image}
;; Produces a function that consumes a color and generates an image of a star with the given
;; `point-count`, the number of points the star has.
(define (render-tile/nstar point-count color)
  (define inner-radius (/ (*game-size*) 4))
  (define outer-radius (*game-size*))
  (radial-star point-count inner-radius outer-radius tile-shape-mode color))

#; {Color -> Image}
;; Produces an image of a 4-side star with the given color.
(define (render-tile/star color)
  (define point-count 4)
  (render-tile/nstar point-count color))

#; {Color -> Image}
;; Produces an image of a 8-side star with the given color.
(define (render-tile/8star color)
  (define point-count 8)
  (render-tile/nstar point-count color))

#; {Color -> Image}
;; Produces an image of a square with the given color.
(define (render-tile/square color)
  (define side-length (*game-size*))
  (square side-length tile-shape-mode color))

#; {Color -> Image}
;; Produces an image of a circle with the given color.
(define (render-tile/circle color)
  (define radius (*game-size*))
  (circle radius tile-shape-mode color))

#; {Color -> Image}
;; Produces an image of a circle with the given color.
(define (render-tile/diamond color)
  (define side-length (*game-size*))
  (define angle 90)
  (rhombus side-length angle tile-shape-mode color))

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

#; {TileShape -> Color -> Image}
;; Given a TileShape, produce a function that takes in a color string and renders an image with the
;; given shape and color.
(define (render-tile/shape-function tile-shape)
  (match tile-shape
    ['star    render-tile/star]
    ['8star   render-tile/8star]
    ['square  render-tile/square]
    ['circle  render-tile/circle]
    ['diamond render-tile/diamond]))

#; {Tile -> Image}
;; Produce an image of the tile, clearly displaying its shape and color. Normalizes images produced
;; by helper functions to be the same width.
(define (render-tile tile)
  (define shape-function (~> tile tile-shape render-tile/shape-function))
  (define color (tile-color tile))
  (render-tile/normalize (shape-function color)))


(module+ examples
  (define red-square-tile (tile++ #:shape 'square
                                  #:color 'red))
  (define blue-star-tile (tile++ #:shape 'star
                                 #:color 'blue)))

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