#lang racket

(provide
 (contract-out
  #:unprotected-submodule no-contract
  [hash-map/make-values-symbols (hash? . -> . hash?)]))

#; {[HashTable K V] -> [HashTable K V]}
;; Shallowly transforms any string values in a hashtable to symbols, and keeps non-string values the
;; same.
(define (hash-map/make-values-symbols ht)
  (define (make-value-symbol key value)
    (values key
            (if (string? value)
                (string->symbol value)
                value)))
  (hash-map/copy ht make-value-symbol))

(module+ test
  (require rackunit)

  (test-equal?
   "empty hash"
   (hash-map/make-values-symbols (hash))
   (hash))

  (test-equal?
   "one key -> one string"
   (hash-map/make-values-symbols (hash 'someKey "someString"))
   (hash 'someKey 'someString))

  (test-equal?
   "one key -> one number"
   (hash-map/make-values-symbols (hash 'someNumber 5))
   (hash 'someNumber 5))

  (test-equal?
   "two keys -> string and number"
   (hash-map/make-values-symbols (hash 'someKey "someString"
                                       'someNumber 5))
   (hash 'someKey 'someString
         'someNumber 5))

  (test-equal?
   "nested hash -- check only shallow"
   (hash-map/make-values-symbols (hash 'someKey "someString"
                                       'someHash (hash 'notTouched "doNotTouch")))
   (hash 'someKey 'someString
         'someHash (hash 'notTouched "doNotTouch"))))
