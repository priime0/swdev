#lang racket

(require racket/class)

(require Q/Common/turn-action)
(require Q/Common/interfaces/playable)
(require Q/Common/interfaces/serializable)
(require Q/Lib/connection)

(provide player-proxy%)

#; {class PlayerProxy}
;; A PlayerProxy acts as a proxy for the referee to interact with remote players via JSON. If the
;; PlayerProxy receives malformed data, then it will error.
(define player-proxy%
  (class* object% (playable<%>)
    (super-new)
    (init-field conn)
    (init-field id)

    (define/public (name)
      (symbol->string id))

    #; {Symbol Any ... -> JSExpr}
    ;; Serialize a complete method call with its list of arguments.
    (define (serialize-call mname . args)
      (cons (->jsexpr mname)
            (->jsexpr args)))

    #; {Symbol Any ... -> JSExpr}
    ;; Send an RPC to the remote player, and return the JSON result of
    ;; the RPC, blocking until a result is returned.
    (define (send-rpc mname . args)
      (define msg (serialize-call mname args))
      (conn-write conn msg)
      (conn-read conn))

    (define/public (setup pub-state hand)
      (define result (send-rpc 'setup pub-state hand))
      (hash->void result))

    (define/public (take-turn pub-state) 
      (define result (send-rpc 'take-turn pub-state))
      (hash->turn-action result))

    (define/public (new-tiles tiles)
      (define result (send-rpc 'new-tiles tiles))
      (hash->void result))

    (define/public (win won?)
      (define result (send-rpc 'win won?))
      (hash->void result))))

#; {JSExpr -> Void}
;; Deserialize a "void" RPC result into a racket #<void> value,
;; erroring if the received json was not "void".
(define (hash->void v)
  (match v
    ["void" (void)]))


(module+ test
  (require rackunit)
  (require Q/Common/map)
  (require Q/Common/game-state)
  (require Q/Common/tile)
  (require Q/Common/posn)
  (require Q/Common/player-state)

  (define hand0 (list (tile 'red 'star)))
  (define pub-state0 (game-state (make-board (tile 'red 'square))
                                 2
                                 (list (make-player-state hand0)
                                       0)))
  (define pment0 (place (list (placement (posn -1 0) (tile 'red 'star)))))

  (define result0 (serialize (void)))
  (define result1 (serialize pment0))
  (define result2 (serialize (void)))

  ;; mock connection setup
  (define mock-input0 (open-input-string (string-append result0 result1 result2)))
  (define mock-output0 (open-output-string))
  (define mock-connection (connection mock-input0 mock-output0))

  ;; player proxy
  (define player-proxy0 (new player-proxy% [id 'andrey] [conn mock-connection])))

(module+ test
  (test-case
   "player proxy writes correct data to connection and returns correct results"
   (send player-proxy0 setup pub-state0 hand0) ;; TODO: can't test because of void

   (test-equal?
    "take turn returns correct placements"
    (send player-proxy0 take-turn pub-state0)
    pment0)

   (send player-proxy0 win #t) ;; TODO: can't test because of void

   (test-equal?
    "player proxy writes correct data to connection"
    (get-output-string mock-output0)
    (string-append (serialize (list 'setup (list pub-state0 hand0)))
                   "\n"
                   (serialize (list 'take-turn (list pub-state0)))
                   "\n"
                   (serialize (list 'win (list #t)))
                   "\n"))))
