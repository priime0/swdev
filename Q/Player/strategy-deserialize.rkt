#lang racket

(require Q/Player/strategy)
(require Q/Player/dag)
(require Q/Player/ldasg)

(provide hash->strategy++)

#; {JSExpr -> Strategy}
(define (hash->strategy++ h)
  (match h
    ["dag"   (new dag%)]
    ["ldasg" (new ldasg%)]))
