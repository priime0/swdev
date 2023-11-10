#lang racket

(require 2htdp/image)

(require Q/Common/config)
(require Q/Lib/contracts)

(provide/cond-contract
 [empty-space
  (-> (and/c real? (not/c negative?))
      (and/c real? (not/c negative?))
      image?)])

(define (empty-space width height)
  (rectangle width height 'solid (*background-color*)))
