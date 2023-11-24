#lang racket

(require Q/Common/config)
(require Q/Lib/result)

(require racket/class)
(require racket/sandbox)
(require (for-syntax syntax/parse)
         (for-syntax racket/syntax))


(provide for/partition
         send/checked
         override-method/exn
         override-method/count)


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
           (λ (e)
             (eprintf "error: ~a\n" (exn-message e))
             (failure payload))])
       (success
        (call-with-limits (*timeout*) #f
                          (thunk
                           (send obj-expr method-id arg ...)))))]))


;; Produces a mixin that consumes and produces a class of the given
;; interface, overriding the given method with its arguments to
;; instead throw an exception.
(define-syntax override-method/exn
  (syntax-rules ()
    [(_ interface-expr exn-method)
     (mixin (interface-expr) (interface-expr)
       (super-new)
       (define/override (exn-method . args)
         (error (quote exn-method) "this is an expected exception")))]))


;; Produces a mixin that consumes and produces a class of the given
;; interface, overriding the given method that runs the super class's
;; method _cnt_ times before looping infinitely on the _cnt_th call.
(define-syntax (override-method/count stx)
  (syntax-parse stx
    [(_ interface-expr exn-method cnt)
     #:with super-exn (format-id stx "super/~a" #'exn-method)
     #'(mixin (interface-expr) (interface-expr)
         (super-new)
         (init-field [curr-count cnt])
         (inherit/super [super-exn exn-method])

         (define/override (exn-method . args)
           (set! curr-count (sub1 curr-count))
           (cond [(zero? curr-count)
                  (let loop () (loop))]
                 [else
                  (super exn-method . args)])))]))
