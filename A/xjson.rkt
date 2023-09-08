#lang racket

(require json)
(require threading)

#; {type JSON = (U Number
                   String
                   Boolean
                   'null
                   [Listof JSON]
                   [HashTable Symbol JSON])}

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
;; is of the type (U Number String). Boolean values and `'null` are ignored.
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
     (xjson/extract sorted-values)]
    [_ '()]))

#; {InputPort -> String}
;; Read and parse a JSON string from an input port into concatenated string of terminating JSON
;; values.
;; ASSUME that the received JSON string is well-formed.
(define (xjson input-port)
  (define hash-table (read-json input-port))
  (define listof-values (xjson/extract hash-table))
  (string-join listof-values ", "))

(module+ main
  (displayln (xjson (current-input-port))))

(module+ test
  (require rackunit))

(module+ test
  (test-equal?
   "single number"
   (xjson (open-input-string "1"))
   "number")

  (test-equal?
   "single string"
   (xjson (open-input-string "\"abc\""))
   "abc")

  (test-equal?
   "single boolean"
   (xjson (open-input-string "true"))
   "")

  (test-equal?
   "single null"
   (xjson (open-input-string "null"))
   "")

  (test-equal?
   "empty list"
   (xjson (open-input-string "[]"))
   "")

  (test-equal?
   "empty object"
   (xjson (open-input-string "{}"))
   "")

  (test-equal?
   "list with single boolean"
   (xjson (open-input-string "[false]"))
   "")

  (test-equal?
   "object with single boolean"
   (xjson (open-input-string "{ \"key\": true }"))
   "")

  (test-equal?
   "list of values"
   (xjson (open-input-string "[\"a\", 1]"))
   "a, number")

  (test-equal?
   "list of values with null and booleans"
   (xjson (open-input-string "[\"a\", 1, null, \"b\", false, \"c\"]"))
   "a, number, b, c")

  (test-equal?
   "single number string"
   (xjson (open-input-string "\"1\""))
   "1")

  (test-equal?
   "non-recursive json"
   (xjson (open-input-string "{\"a\": 1, \"b\": \"abc\"}"))
   "number, abc")

  (test-equal?
   "recursive json"
   (xjson (open-input-string "{\"a\": {\"a\": \"def\", \"c\": 1}, \"b\": \"abc\"}"))
   "def, number, abc")

  (test-equal?
   "recursive json with out-of-order keys"
   (xjson (open-input-string "{\"a\": {\"c\": \"def\", \"a\": 1}, \"b\": \"abc\"}"))
   "number, def, abc")

  (test-equal?
   "json with number as key"
   (xjson (open-input-string "{\"1\": {\"c\": \"def\", \"a\": 1}, \"b\": \"abc\"}"))
   "number, def, abc")

  (test-equal?
   "list with objects and values"
   (xjson (open-input-string "[\"abc\",{\"abb\":\"2\",\"abc\":1}]"))
   "abc, 2, number"))
