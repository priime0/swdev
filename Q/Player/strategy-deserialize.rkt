#lang racket

(require Q/Player/iterative)
(require Q/Player/dag)
(require Q/Player/ldasg)

(provide hash->strategy++)

#; {JSExpr -> Strategy}
(define (hash->strategy++ h)
  (match h
    ["dag"   (new iterative% [s (new dag%)])]
    ["ldasg" (new iterative% [s (new ldasg%)])]))
