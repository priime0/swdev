#lang racket

(require racket/sandbox)
(require Q/Lib/result)

(provide
 (contract-out
  [with-timeout (-> (-> any) exact-nonnegative-integer? any)]))

#; {(-> Any) ExactNonnegativeInteger -> [Result Any Any]}
;; Runs the given thunk with the given time limit in seconds.
(define (with-timeout proc time-limit)
  (with-handlers ([exn:fail:resource? (thunk* (failure #f))])
    (success (call-with-limits time-limit #f proc))))

