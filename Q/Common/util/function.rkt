#lang racket

(provide fjoin)

;; Curried function that applies the given functions to the given arguments.
(define ((fjoin . fns) . args)
  (apply values
         (map (lambda (f) (apply f args))
              fns)))
