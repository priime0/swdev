#lang racket

(require json)

(require Q/Lib/json)

(provide
 (struct-out connection)
 (contract-out
  [conn-write (-> connection? any/c void?)]
  [conn-read  (-> connection? any)]
  [conn-read/timeout (-> connection? (and/c real? (not/c negative?)) any)]
  [create-tcp-connection (-> tcp-listener? connection?)]
  [close-connection (-> connection? void?)]))


#; {type Connection = (connection InputPort OutputPort)}
;; A Connection represents a container for the data necessary for remote communication.
(struct connection (input output) #:transparent)

#; {Connection JSExpr -> Void}
;; Write JSON into the connection.
(define (conn-write conn message)
  (json-write+flush message (connection-output conn)))

#; {Connection -> JSExpr}
;; Read JSON from the connection.
(define (conn-read conn)
  (read-json (connection-input conn)))

#; {Connection NonnegativeReal -> (U JSExpr #f)}
;; Read a JSON from the connection, or return #f if nothing came
;; within the timeout window.
(define (conn-read/timeout conn timeout)
  (define msg #f)
  (sync/timeout
   timeout
   (thread (thunk (set! msg (conn-read conn)))))
  msg)

#; {TcpListener -> Connection}
;; Create a connection from accepting a TCP listener.
;; Blocks until a connection request comes in.
(define (create-tcp-connection listener)
  (define-values (in out) (tcp-accept listener))
  (connection in out))


#; {Connection -> Void}
;; Close the given connection.
(define (close-connection conn)
  (close-input-port (connection-input conn))
  (close-output-port (connection-output conn)))
