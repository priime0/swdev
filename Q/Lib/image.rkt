#lang racket

(require 2htdp/image)

(require Q/Common/config)

(provide
 (contract-out
  [empty-space
   (-> (and/c real? (not/c negative?))
       (and/c real? (not/c negative?))
       image?)]))

(define (empty-space width height)
  (rectangle width height 0 (*background-color*)))
