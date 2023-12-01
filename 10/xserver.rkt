#lang racket

(require Q/Common/config)
(require Q/Server/server)

(require json)

(provide main)

(define (main)
  (define port
    (command-line
     #:args (port-num)
     port-num))

  (define server-config-json (read-json))
  (define config (hash->server-config server-config-json))

  (match-define [server-config _port tries wait signup-wait server-quiet? ref-spec] config)
  (match-define [referee-config state0 ref-quiet? qbo fbo per-turn observe?] ref-spec)

  (parameterize ([*tries*                 tries]
                 [*signup-timeout*        wait]
                 [*server-client-timeout* signup-wait]
                 [*server-quiet?*         server-quiet?]
                 [*points-per-q*          qbo]
                 [*bonus*                 fbo]
                 [*per-turn*              per-turn])
    (run (port))))

(module+ main
  (main))
