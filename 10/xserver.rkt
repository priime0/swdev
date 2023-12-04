#lang racket

(require Q/Common/config)
(require Q/Server/server)
(require Q/Referee/visual-observer)
(require Q/Lib/json)

(require json)

(provide main)

(define (main)
  (define port
    (string->number
     (command-line
      #:args (port-num)
      port-num)))

  (define server-config-json (read-json))
  (define config (hash->server-config server-config-json))

  (match-define [server-config _port tries wait signup-wait server-quiet? ref-spec] config)
  (match-define [referee-config state0 ref-quiet? qbo fbo per-turn observe?] ref-spec)
  
  (define game-result
    (parameterize ([*tries*                 tries]
                   [*signup-timeout*        wait]
                   [*server-client-timeout* signup-wait]
                   [*server-quiet?*         server-quiet?]
                   [*start-state*           state0]
                   [*points-per-q*          qbo]
                   [*bonus*                 fbo]
                   [*per-turn*              per-turn]
                   [*referee-quiet?*        ref-quiet?])
      (when observe?
        (send (*obman*) connect (new default-observer%)))
      (run port)))
  (json-write+flush game-result))

(module+ main
  (main)
  #;
  (parameterize ([current-custodian (make-custodian)])

    (custodian-shutdown-all (current-custodian))))
