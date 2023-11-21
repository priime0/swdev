#lang racket

(require racket/generic)

(require json)

(provide
 gen:serializable
 serializable?
 serializable/c
 ->jsexpr
 serialize)

;; A Serializable is an interface that implements transformation to a hash table and transformation
;; to a JSON string.
(define-generics serializable
  #; {Serializable -> JSExpr}
  ;; Transform the given serializable into a hashtable representing a JSON.
  (->jsexpr serializable)
  #:defaults
  ([boolean?
    (define (->jsexpr b)
      b)]
   [list?
    (define/generic ->jsexpr* ->jsexpr)
    (define (->jsexpr lst)
      (map ->jsexpr* lst))]
   [void?
    (define (->jsexpr v)
      "void")]
   [string?
    (define (->jsexpr s)
      s)]
   [symbol?
    (define (->jsexpr s)
      (symbol->string s))]))

#; {Serializable -> String}
;; Transform the given serializable into its JSON string representation.
(define (serialize s)
  (jsexpr->string (->jsexpr s)))
