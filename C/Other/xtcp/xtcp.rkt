#lang racket

(require json)
(require threading)

#; {type JSON = (U Number
                   String
                   [Listof JSON]
                   [HashTable Symbol JSON])}

#; {type TCPPort = Natural}
;; A TCPPort represents a possible port for the TCP connection in `xtcp`.
;; CONSTRAINT TCPPort is a natural between [10000, 60000]

#; {[Listof [Pair Symbol JSON]] -> [Listof [Pair Symbol JSON]]}
;; Sort an alist lexicographically by the symbol key.
(define (sort-assoc-list alist)
  (sort alist
        string<?
        #:cache-keys? #t
        #:key (compose1 symbol->string car)))

#; {JSON -> [Listof String]}
;; Recursively extract terminating JSON values as strings, collecting the extracted values into a
;; flattened list. Number values are transformed into the string "number". A terminating JSON value
;; is of the type (U Number String). 
(define (xjson/extract json)
  (match json
    [(? number?) '("number")]
    [(? string?) `(,json)]
    [(? list?)
     (~>> json
          (map xjson/extract)
          flatten)]
    [(? hash?)
     (define sorted-assoc-list
       (~> json
           hash->list
           sort-assoc-list))
     (define sorted-values
       (map cdr sorted-assoc-list))
     (xjson/extract sorted-values)]))

#; {JSON -> String}
;; Parse the given JSON into a concatenated string of terminating JSON values.
;; ASSUME that the received JSON string is well-formed.
(define (xjson json-string)
  (define listof-values (xjson/extract json-string))
  (string-join listof-values ", "))

#; {TCPPort -> Void}
;; Read and parse JSON strings sent over a client-server TCP connection running on the given port number.
;; ASSUME that all received JSON strings are well-formed.
(define (xtcp port-number)
  (define tcp-listener (tcp-listen port-number))
  (define-values (in out) (tcp-accept tcp-listener))
  (let loop ()
    (define json (read-json in))
    (unless (eof-object? json)
      (define result (xjson json))
      (println result out)
      (flush-output out)
      (loop)))
  (tcp-close tcp-listener))

(module+ main
  (match-define (vector port-num-str) (current-command-line-arguments))
  (define port-num (string->number port-num-str))
  (xtcp port-num))

