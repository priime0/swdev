#lang racket

(provide
 (contract-out
  #:unprotected-submodule no-contract
  [member? (any/c (or/c list? any/c) . -> . boolean?)]
  [singleton? (list? . -> . boolean?)]
  [all-same? (list? . -> . boolean?)]))

#; {(X) X [Listof X] -> Boolean}
;; Is the given element `v` a member of the given list `lst`?
(define (member? v lst)
  (cons? (member v lst)))

#; {(X) [Listof X] -> Boolean}
;; Is the given list a singleton? That is, is its length 1?
(define (singleton? lst)
  (and (pair? lst)
       (null? (cdr lst))))

#; {(X) [Listof X] -> Boolean}
;; Are all elements in the list `equal?`
(define (all-same? lst)
  (or (null? lst)
      (andmap (curry equal? (first lst))
              (rest lst))))

(module+ test
  (require rackunit)

  (test-equal?
   "x not member"
   (member? 'x '(a b c))
   #f)

  (test-equal?
   "y is member"
   (member? 'y '(x y z))
   #t))
