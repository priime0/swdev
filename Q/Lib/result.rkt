#lang racket

(require Q/Lib/contracts)

(provide
 (struct-out success)
 (struct-out failure))

(provide/cond-contract
 [unwrap-or
  (-> result?
      any/c
      any)]
 [unwrap-or-else
  (-> result?
      procedure?
      any)])


#; {type [Result X Y] (U [Success X] [Failure Y])}
;; A type used for returning and propogating errors or lack of errors algebraically.
;; A replacement for exceptions.
(struct success (val) #:transparent)
(struct failure (val) #:transparent)

(define result? (or/c success? failure?))


#; {[Result X Y] X -> X}
;; Return the contained success value, otherwise the fallback value given.
(define (unwrap-or res fallback)
  (match res
    [(success v) v]
    [(failure _) fallback]))


#; {[Result X Y] [-> X] -> X}
;; Return the contained success value, otherwise produce and return the value of
;; the function.
(define (unwrap-or-else res fn)
  (match res
    [(success v) v]
    [(failure _) (fn)]))
