#lang racket

(provide list->count)

#; {[Listof X] -> [HashTable X Natural]}
;; Create a counter of the elements in the given list.
(define (list->count lst)
  (define counter (make-hash))
  (for ([el lst])
    (hash-update! counter el add1 0))
  counter)

(module+ test
  (require rackunit))

(module+ test

  (test-equal?
   "empty list"
   (list->count '())
   (make-hash))

  (test-equal?
   "non-empty list with repeated values"
   (list->count '(1 2 1 3 4))
   (make-hash '((1 . 2) (2 . 1) (3 . 1) (4 . 1)))))

