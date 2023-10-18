#lang racket

(provide
 fjoin
 flip)

;; Curried function that applies the given functions to the given arguments.
(define ((fjoin . fns) . args)
  (apply values
         (map (lambda (f) (apply f args))
              fns)))

#; {(X,Y,Z) (X Y -> Z) -> (Z Y -> X)}
;; Produces a binary function that flips the arguments of a binary function.
(define ((flip f) x y)
  (f y x))
