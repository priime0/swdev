#lang racket

(require rackunit)

(provide
 test-values-equal?
 apply/seed)

(define-syntax test-values-equal?
  (syntax-rules ()
    [(_ msg a b) (test-equal? msg
                              (call-with-values (thunk a) list)
                              (call-with-values (thunk b) list))]))


#; {Natural Procedure Any ... -> Any ...}
;; Applies the given function using a random generator parameterized by the given seed, producing a
;; deterministic result
(define (apply/seed seed f . args)
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (apply f args)))
