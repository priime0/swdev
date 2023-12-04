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
  [start (-> (is-a?/c playable<%>) void?)]))

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
(define (start playable)
  (define maybe-conn (try-signups playable))
  (when maybe-conn
    (with-handlers ([exn:fail? (lambda (e) (unless (*client-quiet?*) (eprintf "~a\n" e)))])
      (listen maybe-conn playable))))

#; {Playable -> [Maybe Connection]}
;; Tries at most *client-retries* times to connect to the server, or
;; returns false if unable.
(define (try-signups playable)
  (let loop ([remaining-attempts (*client-retries*)])
    (with-handlers ([exn:fail:network?
                     (thunk* (unless (*client-quiet?*)
                               (eprintf "unable to connect to ~a on port ~a\n"
                                        (*hostname*)
                                        (*port*)))
                             (sleep (*client-wait-before-retry*))
                             (loop (sub1 remaining-attempts)))])
      (cond
        [(zero? remaining-attempts) #f]
        [else
         (define conn (connect (*hostname*) (*port*)))
         (sign-up conn playable)
         conn]))))


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
