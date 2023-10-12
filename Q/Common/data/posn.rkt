#lang racket

(require racket/hash)

(require struct-plus-plus)

(require Q/Common/util/list)
(require Q/Common/util/struct)

(provide
 posn
 posn++
 posn?
 posn-row
 posn-column
 directions
 vertical-axis
 horizontal-axis
 direction-names
 direction-name?
 (contract-out
  #:unprotected-submodule no-contract
  [posn-translate
   (-> posn? direction-name? posn?)]
  [neighbors?
   (-> posn? posn? boolean?)]
  [posn-neighbors/dirs
   (->i ([p posn?] [dirs (listof direction-name?)])
        [result (p) (listof (flat-contract (curry neighbors? p)))])]
  [posns-same-row?
   (-> (listof posn?) boolean?)]
  [posns-same-column?
   (-> (listof posn?) boolean?)]
  [same-axis?
   (-> (listof posn?) boolean?)]))

#; {type Posn = (posn Integer Integer)}
;; A Posn is a (posn r c), which represent a row and column.
(struct++ posn
          ([row    integer?]
           [column integer?])
          (#:convert-from
           (hash
            [hash?
             (hash-table ('row row) ('column column))
             (row column)]))
          #:transparent)


#; {type Direction = (U 'up 'down 'left 'right)}
;; A Direction represents a pair of '(Δr . Δc) that are unit vector translations in one of the
;; four distinct directions in a square grid.
;; A Direction thus has two axes, vertical and horizontal. A Direction is a translation in only one
;; axis. For the vertical axis, up is negative and down is positive. For the horizontal axes, left
;; is negative and right is positive.
;; An Axis is a 2-list of distinct directions (list α β) such that, for any position φ,
;; (posn-translate (posn-translate φ α) β) = φ
(define vertical-directions '((up . (-1 . 0)) (down . (1 . 0))))
(define vertical-axis (map car vertical-directions))

(define horizontal-directions '((left . (0 . -1)) (down . (0 . 1))))
(define horizontal-axis (map cdr horizontal-directions))

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
  (match-define [posn row column] p)
  (match-define [cons dr dc] (hash-ref directions dir))
  (posn (+ row dr)
        (+ column dc)))


#; {Posn [Listof Direction] -> [Listof Posn]}
;; Produces the list of neighboring posns for the given posn for each direction in the given list.
;; Does not filter out duplicate positions.
(define (posn-neighbors/dirs posn dir-list)
  (define posn-translate^ (curry posn-translate posn))
  (map posn-translate^ dir-list))

#; {[Listof Posn] -> Boolean}
;; Does every posn in the given list share the same row?
(define (posns-same-row? posns)
  (struct-equal-field? posn-row posns))

#; {[Listof Posn] -> Boolean}
;; Does every posn in the given list share the same column?
(define (posns-same-column? posns)
  (struct-equal-field? posn-column posns))

#; {[Listof Posn] -> Boolean}
;; Do the posns belong on the same axis?
(define (same-axis? posns)
  ((disjoin posns-same-row? posns-same-column?)
   posns))
