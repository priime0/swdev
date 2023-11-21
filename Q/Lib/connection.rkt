#lang racket

(require json)

(require Q/Lib/json)

(provide
 (struct-out connection)
 (contract-out
  [conn-write (-> connection? any/c void?)]
  [conn-read  (-> connection? any)]))


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
