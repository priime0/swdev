#lang racket

(require racket/hash)

(require struct-plus-plus)

(require Q/Common/util/list)

(provide
 posn
 posn++
 posn?
 posn-row
 posn-column
 directions
 vertical-directions
 horizontal-directions
 direction-names
 direction-name?
 (contract-out
  #:unprotected-submodule no-contract
  [posn-translate
   (-> posn? direction-name? posn?)]
  [neighbors?
   (-> posn? posn? boolean?)]
  [posn-neighbors/dirs
   (-> posn? (listof direction-name?) (listof posn?))]))

#; {type Posn = (posn Integer Integer)}
;; A Posn is a (posn r c), which represent a row and column.
(struct++ posn
          ([row    integer?]
           [column integer?])
          #:transparent)


#; {type Direction = (U 'up 'down 'left 'right)}
;; A Direction is a pair of '(Δr . Δc), representing a unit vector translation in one of the four
;; distinct directions in a square grid.
;; A Direction thus has two axes, vertical and horizontal. A Direction is a translation in only one
;; axis. For the vertical axis, up is negative and down is positive. For the horizontal axes, left
;; is negative and right is positive.
(define vertical-directions
  #hash([up    . (-1 . 0)]
        [down  . (1 . 0)]))

(define horizontal-directions
  #hash([left  . (0 . -1)]
        [right . (0 . 1)]))

(define directions
  (hash-union vertical-directions horizontal-directions))

(define direction-names (hash-keys directions))

#; {Any -> Boolean}
(define (direction-name? a)
  (member? a direction-names))

;; DEFINITION: Two `Posn`s α, β are _neighbors_ IFF WLOG there exists some direction Δ such that
;;             `(posn-translate α Δ)` produces β.
;;             - _neighbors_ is commutative: neighbors(α, β) = neighbors(β, α)
(define (neighbors? a b)
  (for/or ([d direction-names])
    (equal? a (posn-translate b d))))


#; {Posn Direction -> Posn}
;; Produce a new posn representing the given posn translated in the given direction.
(define (posn-translate p dir)
  (match-define [posn row col] p)
  (match-define [cons dr dc] (hash-ref directions dir))
  (posn (+ row dr)
        (+ col dc)))


#; {Posn [Listof Direction] -> [Listof Posn]}
;; Produces the list of neighboring posns for the given posn for each direction in the given list.
;; Does not filter out duplicate positions.
(define (posn-neighbors/dirs posn dir-list)
  (define posn-translate^ (curry posn-translate posn))
  (map posn-translate^ dir-list))
