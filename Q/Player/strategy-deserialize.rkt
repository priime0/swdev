#lang racket

(require Q/Player/iterative)
(require Q/Player/dag)
(require Q/Player/ldasg)
(require Q/Player/cheating-strategy)
(require Q/Player/compound)

(provide hash->strategy++)

#; {JSExpr -> Strategy}
(define (hash->strategy++ h [cheat #f])
  (define base-strat
    (match h
      ["dag"   (new iterative% [strategy (new dag%)])]
      ["ldasg" (new iterative% [strategy (new ldasg%)])]))
  (cond
    [cheat
     (define cheat-strat
       (match cheat
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
     (new compound% [strategy0 cheat-strat] [strategy1 base-strat])]
    [else base-strat]))
