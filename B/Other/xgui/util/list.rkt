#lang racket

(provide
 (contract-out
  #:unprotected-submodule no-contract
  [member? (any/c (or/c list? any/c) . -> . boolean?)]))

#; {(X) X [Listof X] -> Boolean}
;; Is the given element `v` a member of the given list `lst`?
(define (member? v lst)
  (cons? (member v lst)))

(module+ test
  (require rackunit)

  (test-equal?
   ""
   (member? 'x '(a b c))
   #f)

  (test-equal?
   ""
   (member? 'y '(x y z))
   #t))
