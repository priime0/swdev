#lang racket

(require Q/Player/iterative)
(require Q/Player/dag)
(require Q/Player/ldasg)
(require Q/Player/cheating-strategy)
(require Q/Player/compound)

(provide hash->strategy++)

#; {JSExpr JCheat -> Strategy}
(define (hash->strategy++ h [maybe-cheat #f])
  (define base-strat (jstrategy->strategy h))
  (cond
    [maybe-cheat
     (define cheat-strat (jcheat->strategy maybe-cheat))
     (new compound% [strategy0 cheat-strat] [strategy1 base-strat])]
    [else base-strat]))

#; {JStrategy -> Strategy}
;; Deserializes a JStrategy into a PlayerStrategy
(define (jstrategy->strategy jstrategy)
  (match jstrategy
    ["dag"   (new iterative% [strategy (new dag%)])]
    ["ldasg" (new iterative% [strategy (new ldasg%)])]))

#; {JCheat -> Strategy}
;; Converts a JCheat into the corresponding cheating strategy.
(define (jcheat->strategy jcheat)
  (match jcheat
    ["non-adjacent-coordinate"
     (new non-adj-coord%)]
    ["tile-not-owned"
     (new tile-not-owned%)]
    ["not-a-line"
     (new not-a-line%)]
    ["bad-ask-for-tiles"
     (new bad-ask-for-tiles%)]
    ["no-fit"
     (new no-fit%)]))
