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
