#lang racket

(require Q/Common/config)
(require Q/Client/client)
(require Q/Player/iterative)
(require Q/Player/ldasg)
(require Q/Player/player)

(require json)

(provide main)

(define (main)
  (define player-name (make-parameter 'foo))

  (define port
    (command-line
     #:args (port-num)
     port-num))

  (define client-config-json (read-json))
  (define config (hash->client-config client-config-json))

  (match-define [client-config _port host wait quiet? players] config)

  (parameterize ([*port* port]
                 [*hostname* host]
                 [*wait* wait])
    (start players)))


(module+ main
  (main))
