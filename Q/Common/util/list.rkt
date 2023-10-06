#lang racket

(require Q/Common/util/hash)

(provide
 (contract-out
  #:unprotected-submodule no-contract
  [member?       (any/c (or/c list? any/c) . -> . boolean?)]
  [all-same?     (list? . -> . boolean?)]
  [rotate-left   (natural? list? . -> . list?)]
  [rotate-left-1 (list? . -> . list?)]
  [remove-from   (list? list? . -> . list?)]
  [contains-all? (list? list? . -> . boolean?)]))

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

#; {(X) [Listof X] [Listof X] -> Boolean}
;; Does `l1` contain every element of `l2`?
(define (contains-all? l1 l2)
  (define l1-counter (list->count l1))
  (define l2-counter (list->count l2))

  (for/and ([el (hash-keys l2-counter)])
    (and (hash-has-key? l1-counter el)
         (<= (hash-ref l2-counter el) (hash-ref l1-counter el)))))

#; {(X) [Listof X] [Listof X] -> [Listof X]}
;; Remove the elements in `l1` from `l2`.
(define (remove-from l1 l2)
  (define expected-length (- (length l2) (length l1)))

  (when (negative? expected-length)
    (error 'remove-from
           "l1 is greater in size than l2"))

  (define counter (list->count l1))
  (define-values (pruned-list pruned-list-length)
    (for/fold ([lst '()]
               [len 0]
               #:result (values (reverse lst) len))
              ([el l2])
      (cond [(not (zero? (hash-ref counter el 0)))
             (hash-update! counter el sub1)
             (values lst len)]
            [else
             (values (cons el lst) (add1 len))])))

  (unless (= pruned-list-length expected-length)
    (error 'remove-from
           "l1 is not contained in l2"))

  pruned-list)

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
