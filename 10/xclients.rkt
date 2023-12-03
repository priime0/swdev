#lang racket

(require Q/Common/config)
(require Q/Client/client)
(require Q/Player/iterative)
(require Q/Player/ldasg)
(require Q/Player/player)

(require json)

(provide main)

(define (main)
  (define port
    (string->number
     (command-line
      #:args (port-num)
      port-num)))

  (define client-config-json (read-json))
  (define config (hash->client-config client-config-json))

  (match-define [client-config _port host wait quiet? players] config)

  (parameterize ([*port* port]
                 [*hostname* host]
                 [*wait* wait]
                 [*client-quiet?* quiet?])
    (run-players players)))

(define (run-players players)
  (define threads
    (for/fold ([threads '()])
              ([player  players])   
      (define t (thread (thunk (println (send player name))
                               (flush-output) (start player))))
      (sleep (*wait*))
      (cons t threads)))
  (for-each thread-wait threads))

(module+ main
  (main))
