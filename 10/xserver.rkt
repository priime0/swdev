#lang racket

(require Q/Server/server)

(provide main)

(define (main)
  (define port (make-parameter 10000))

  (command-line
   #:once-each
   ["--port" port-str
             "Port"
             (port (string->number port-str))])

  (run (port)))

(module+ main
  (main))
