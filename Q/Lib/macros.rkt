#lang racket

(require Q/Common/config)
(require Q/Lib/result)

(require racket/class)
(require racket/sandbox)
(require (for-syntax syntax/parse)
         (for-syntax racket/syntax))


(provide for/partition
         send/checked
         override-method/exn)


;; Iterates through the given sequence, adding an element to the `accept' list
;; if the body produces a truish value, and otherwise adding the element to the
;; `reject' list.
;;
;; Example:
;;
;; > (for/partition ([i (in-range 10)])
;;     (even? i))
;; '(0 2 4 6 8)
;; '(1 3 5 7 9)
(define-syntax for/partition
  (syntax-rules ()
    [(for/partition ([id seq-expr] ...) body ...)
     (for/fold ([accepts '()]
                [rejects '()]
                #:result (values (reverse accepts) (reverse rejects)))
               ([id seq-expr] ...)
       (define v (begin body ...))
       (define ids
         (if (= 1 (length (list id ...)))
             (first (list id ...))
             (list id ...)))
       (if v
           (values (cons ids accepts) rejects)
           (values accepts (cons ids rejects))))]))


;; Calls the method on the given object with the given arguments, producing the
;; payload if an exception is raised or timeout occurs. Returns a Result type.
(define-syntax send/checked
  (syntax-rules ()
    [(send/checked obj-expr method-id payload arg ...)
     (with-handlers
         ([exn:fail?
           (λ (e) (failure payload))])
       (success
        (call-with-limits (*timeout*) #f
                          (thunk
                           (send obj-expr method-id arg ...)))))]))

;; Produces a mixin that consumes and produces a class of the given
;; interface, overriding the given method with its arguments to
;; instead throw an exception.
(define-syntax (override-method/exn stx)
  (syntax-parse stx
    [(_ interface-expr exn-method method-arg ...)
     #:with exn-name (syntax->datum #'exn-method)
     #'(mixin (interface-expr) (interface-expr)
         (super-new)
         (define/override (exn-method method-arg ...)
           (error (quote exn-name) "this is an expected exception")))]))


