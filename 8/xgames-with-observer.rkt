#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Player/player)
(require Q/Referee/referee)
(require Q/Referee/observer)
(require Q/Lib/json)

(provide main)

(define (main)
  (define show (make-parameter #f))
  (command-line
   #:once-each
   ["--show" "Attach observer to referee"
             (show #t)])

  (define jstate (read-json))
  (define jactors (read-json))

  (define start-state (hash->priv-state jstate))
  (define players (map hash->player++ jactors))

  (if (show)
      (send (*obman*) connect (new default-observer%))
      (void))

  (define result (play-game players #:game-state start-state))
  (define winners (first result))
  (define sinners (second result))

  (define sorted-winners (sort winners string<=?))

  (json-write+flush (list sorted-winners sinners)))

(module+ main
  (main))
