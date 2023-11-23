#lang racket

(require Q/Client/client)
(require Q/Player/iterative)
(require Q/Player/ldasg)
(require Q/Player/player)

(provide main)

(define (main)
  (define port (make-parameter 10000))
  (define player-name (make-parameter 'foo))

  (command-line
   #:once-each
   ["--port" port-str
             "Port"
             (port (string->number port-str))]
   ["--name" name-str
             "Name"
             (player-name (string->symbol name-str))])

  (define playable (new player% [id (player-name)] [strategy (new iterative% [strategy (new ldasg%)])]))
  (start "localhost" (port) playable))


(module+ main
  (main))
