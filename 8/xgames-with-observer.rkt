#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Player/player)
(require Q/Referee/referee)
(require Q/Lib/json)


(module+ main
  (define show (make-parameter #f))
  (command-line
   #:once-each
   ["--show" "Attach observer to referee"
             (show #t)])

  (define jstate (read-json))
  (define jactors (read-json))

  (define start-state (hash->priv-state jstate))
  (define players (map hash->player++ jactors))

  (when (show)
    (define default-observer% (dynamic-require 'Q/Referee/observer
                                               'default-observer%))
    (send (*obman*) connect (new default-observer%)))

  (define result (play-game players #:game-state start-state))
  (define winners (first result))
  (define sinners (second result))

  (define sorted-winners (sort winners string<=?))

  (json-write+flush (list sorted-winners sinners)))
