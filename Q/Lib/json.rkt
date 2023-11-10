#lang racket

(require json)

(provide json-write+flush)

(define (json-write+flush j)
  (write-json j)
  (displayln "")
  (flush-output))
