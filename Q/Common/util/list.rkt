#lang racket

(require Q/Common/util/hash)

(provide
 (contract-out
  #:unprotected-submodule no-contract
  [member?       (any/c (or/c list? any/c) . -> . boolean?)]
  [all-same?     (list? . -> . boolean?)]
  [rotate-left   (natural? list? . -> . list?)]
  [rotate-left-1 (list? . -> . list?)]
  [remove-from   (list? list? . -> . list?)]))

#; {(X) X [Listof X] -> Boolean}
;; Is the given element `v` a member of the given list `lst`?
(define (member? v lst)
  (cons? (member v lst)))

#; {(X) [Listof X] -> Boolean}
;; Are all elements in the list `equal?`
(define (all-same? lst)
  (or (null? lst)
      (andmap (curry equal? (first lst))
              (rest lst))))

#; {(X) Natural [Listof X] -> [Listof X]}
;; Rotates the given list _n_ times, moving the first _n_ elements to the end.
(define (rotate-left n lst)
  (define-values (hd tl) (split-at lst n))
  (append tl hd))

#; {(X) [Listof X] -> [Listof X]}
;; Rotates the given list by one, moving the first element to the end.
(define (rotate-left-1 lst)
  (rotate-left 1 lst))

#; {(X) [Listof X] [Listof X] -> [Listof X]}
;; Remove the elements in `l1` from `l2`.
(define (remove-from l1 l2)
  (define counter (list->count l1))
  (for/fold ([lst '()]
             #:result (reverse lst))
            ([el l2])
    (cond [(not (zero? (hash-ref counter el 0)))
           (hash-update! counter el sub1)
           lst]
          [else (cons el lst)])))

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


(module+ test
  (test-true
   "all number elements are the same"
   (all-same? '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

  (test-false
   "all number elements are not the same"
   (all-same? '(1 1 1 1 1 1 2 1 1 1 1 1 1 1 2)))

  (test-true
    "all string elements are the same"
    (all-same? '("a" "a" "a" "a" "a" "a" "a" "a" "a" "a")))

  (test-false
   "all string elements are not the same"
   (all-same? '("a" "d" "a" "c" "a" "a" "a" "a" "a" "b"))))
