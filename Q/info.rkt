#lang info

(define collection "Q")
(define deps '("base"
               "threading"
               "struct-plus-plus"
               "htdp"
               "functional-lib"
               "predicates"
               "pfds"
               "gui-easy"))
(define build-deps '("racket-doc" "rackunit-lib"))
(define pkg-desc "An implementation of the Q game")
(define version "0.2")
(define pkg-authors '(priime0 AndreyPiterkin))
