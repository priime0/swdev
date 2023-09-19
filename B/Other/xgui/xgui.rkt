#lang racket

(require "data/tile.rkt")
(require "util/hash.rkt")

(require json)
(require struct-plus-plus)
(require threading)
(require 2htdp/image)
(require 2htdp/universe)

#; {InputPort -> Void}
;; Reads JSON from STDIN, generating their corresponding images of tiles and saving them to the
;; specified filepath.
;; The structure of the JSON is [ "file is:", String (FileName), TileSpecification, ...+ ] where
;; TileSpecification is { "shape": TileShape, "color": TileColor }.
(define (xgui [input (current-input-port)] #:show? show?)
  (let loop ()
    (define next-json (read-json input))
    (unless (eof-object? next-json)
      (define filename (second next-json))
      (define tiles-start-position 2)
      (define rendered-image (xgui/render-image (drop next-json tiles-start-position)))
      (when show?
        (xgui/show rendered-image))
      (save-image rendered-image filename)
      (println "done")
      (loop))))

#; {Image -> Void}
;; Opens the rendered image in a window on the user's screen until they press mouse down.
(define (xgui/show rendered-image)
  (big-bang rendered-image
    [to-draw
     (lambda (ws)
       (define width (image-width rendered-image))
       (define height (image-height rendered-image))
       (define base-scene (empty-scene width height))
       (define place-x (/ width 2))
       (define place-y (/ height 2))
       (place-image rendered-image place-x place-y base-scene))]
    [on-mouse
     (lambda (ws x y me)
       (if (equal? me "button-down")
           #f
           ws))]
    [stop-when (lambda (ws) (false? ws))])
  (void))

#; {[Listof TileSpecification] -> Image}
;; Renders every tile beside each other horizontally, producing a single image.
(define (xgui/render-image tile-jsons)
  (define tiles (~>> tile-jsons
                     (map hash-map/make-values-symbols)
                     (map (curry hash->struct++ tile++))))
  (define rendered-tile-images (~>> tiles
                                    (map render-tile)
                                    (map frame)))
  (define tile-images/length>=2 (cons empty-image rendered-tile-images))
  (apply beside tile-images/length>=2))

(module+ main
  (require racket/cmdline)

  (define show? (make-parameter #f))

  (command-line
   #:program "xgui"
   #:once-each
   [("--show")   "Displays the shape in the window"
                 (show? #t)]
   #:args ()
   (void))

  (xgui #:show? (show?)))
