#lang racket

(require Q/Lib/hash)
(require Q/Lib/function)

(provide
 find-remf
 (contract-out
  #:unprotected-submodule no-contract
  [member?       (any/c (or/c list? any/c) . -> . boolean?)]
  [all-same?     (list? . -> . boolean?)]
  [rotate-left   (natural? list? . -> . list?)]
  [rotate-left-1 (list? . -> . list?)]
  [remove-from   (list? list? . -> . list?)]
  [contains-all? (list? list? . -> . boolean?)]
  [same-elements? (list? list? . -> . boolean?)]
  [segment       (natural? list? . -> . list?)]
  [index<?       (list? any/c any/c . -> . boolean?)]
  [sort-by       ((listof (cons/c (any/c any/c . -> . any/c) (any/c . -> . any/c))) . -> . ((listof any/c) . -> . (listof any/c)))]))

#; {(X) X [Listof X] -> Boolean}
;; Is the given element `v` a member of the given list `lst`?
(define (member? v lst)
  (cons? (member v lst)))

#; {(X) [Listof X] X X -> Boolean}
;; Is the index of the first element less than the index of the second element in the given list?
;; ASSUME both items are in the list.
(define (index<? lst el1 el2)
  (< (index-of lst el1)
     (index-of lst el2)))

#; {(X Y) [Listof [Pair (Y Y -> Boolean) (X -> Y)]] -> ([Listof X] -> [Listof X])}
;; Produce a sorting function that sorts some list by the given functions in the list. Each element
;; in the list is a pair of a binary comparison function and accessor function, where the accessor
;; function is applied on each element in the list, then the values produced by the accessor
;; function are compared by the binary comparison function. Elements are ordered from most important
;; comparison-accessor pairs first to least important last.
;; ASSUME: the provided binary comparison functions maintain stable-sorting.
(define (sort-by fkey-asoc)
  (define ((wrap-sort fkey acc-f) lst)
    (match-define [cons f accessor] fkey)
    (sort (acc-f lst)
          f
          #:key accessor))
  (foldl wrap-sort identity (reverse fkey-asoc)))

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


#; {(X) [Listof X] [Listof X] -> Boolean}
;; Do both lists contain the same number of each element?
(define (same-elements? l1 l2)
  (and (contains-all? l1 l2)
       (contains-all? l2 l1)))


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

#; {(X) Natural [Listof X] -> [Listof [Listof X]]}
;; Segments the list into multiple lists of size _size_. If the length of the given list is not
;; divisible by _size_, then the first list in the result will be shorter.
(define (segment size lst)
  (for/foldr ([acc  '()]
              [curr '()]
              [len  0]
              #:result (cons curr acc))
            ([el lst])
    (if (= len size)
        (values (cons curr acc)
                (list el)
                1)
        (values acc
                (cons el curr)
                (add1 len)))))


#; {(X) [X -> Boolean] [Listof X] -> (values X [Listof X])}
;; Find and remove the first element of the given list that satisfies the given predicate, returning
;; both the removed element and the new list.
(define (find-remf pred lst)
  ((fjoin findf remf)
   pred lst))


#; {(X) [X -> Boolean] X [Listof X] -> [Listof X]}
;; `cons` the given item to the given list if it satisfies the given predicate.
(define (cons-if pred? x lst)
  (if (pred? x)
      (cons x lst)
      lst))


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
