#lang racket

(require Q/Lib/connection)
(require Q/Client/referee)
(require Q/Common/config)
(require Q/Common/interfaces/playable)
(require Q/Player/player)

(provide
 (struct-out client-config)
 (contract-out
  [hash->client-config (-> hash? client-config?)]
  [start (-> (listof (is-a?/c playable<%>)) void?)]))

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {type ClientConfig = (client-config Natural String Natural Boolean [Listof Playable])}
;; Client configuration
(struct/contract client-config 
  ([port natural?]
   [host string?]
   [wait natural?]
   [quiet? boolean?]
   [players (listof (is-a?/c playable<%>))])
  #:transparent)

;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {Playable -> Void}
;; Connects to the server and signs up the player, participating in a game.
(define (start playables)
  (define conn (connect (*hostname*) (*port*)))
  (define one-done? #f)
  (for ([playable playables])
    (sign-up conn playable)
    (listen conn playable)
    (when one-done? (sleep (*wait*)))
    (set! one-done? #t)))


#; {Connection Playable -> Void}
;; Signs up to the server through the given connection, retrieving and sending the player's name.
(define (sign-up conn playable)
  (define name (send playable name))
  (conn-write conn name))


#; {JSExpr -> ClientConfig}
;; Validates the structure of a ClientConfig JSON as it conforms to the spec.
(define (hash->client-config jsexpr)
  (define required-keys '(port host wait quiet players))
  (define has-all-keys?
    (andmap (curry hash-has-key? jsexpr) required-keys))
  (unless has-all-keys?
    (error 'hash->client-config "missing fields for client config"))

  (define ref (curry hash-ref jsexpr))

  (define port (ref 'port))
  (define host (ref 'host))
  (define wait (ref 'wait))
  (define quiet (ref 'quiet))
  (define players (map hash->player++ (ref 'players)))

  (client-config port host wait quiet players))
