#lang racket

(require json)

(provide json-write+flush)

(define (json-write+flush j [port (current-output-port)])
  (write-json j port)
  (displayln "      " port)
  (flush-output port))
