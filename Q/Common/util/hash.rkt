#lang racket

(provide list->count)

#; {[Listof X] -> [HashTable X Natural]}
;; Create a counter of the elements in the given list.
(define (list->count lst)
  (define counter (make-hash))
  (for ([el lst])
    (hash-update! counter el add1 0))
  counter)
