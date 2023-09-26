#lang racket

(require Q/Common/util/list)

(provide
 struct-equal-field?)

#; {(X Y) (X -> Y) [Listof X] -> Boolean}
;; where X is a struct
;; Are the values produced by the accessor for each instance of some common struct all the same?
(define (struct-equal-field? accessor instances)
  (all-same? (map accessor instances)))
