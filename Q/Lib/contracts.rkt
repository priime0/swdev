#lang racket/base
;; ---------------------------------------------------------------------------------------------------
(require (for-syntax racket/base racket/string)
         racket/contract
         racket/require-syntax racket/provide-syntax
         racket/match
         racket/list
         syntax/parse/define
         racket/struct-info)

(provide provide/cond-contract
         compiled-with-contracts?
         (all-from-out racket/contract))

;; ---------------------------------------------------------------------------------------------------
;;
;; Enables contracts optionally, depending on the value of ENABLE_CONTRACTS.
;;
;; This work was based on the typed-racket implementation of optional contracts:
;; https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/utils/utils.rkt
(define-for-syntax enable-contracts?
  (and (getenv "ENABLE_CONTRACTS") #false))

(begin-for-syntax
  (define-syntax-class clause
    #:attributes (i)
    (pattern [(~datum struct) (~or nm:id (nm:id super:id)) (flds ...)]
      #:with i #'(struct-out nm))
    (pattern [(~datum rename) out:id in:id cnt:expr]
      #:with i #'(rename-out [out in]))
    (pattern [i:id cnt:expr])))

(define-syntax provide/cond-contract
  (if enable-contracts?
      (lambda (stx)
        (syntax-parse stx
          [(_ c:clause ...)
           #'(provide (contract-out c ...))]))
      (lambda (stx)
        (syntax-parse stx
          [(_ c:clause ...)
           #'(provide c.i ...)]))))

(define-syntax (compiled-with-contracts? stx)
  (datum->syntax stx enable-contracts?))
