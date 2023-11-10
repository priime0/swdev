#lang racket

(require racket/hash)

(require struct-plus-plus)

(require Q/Lib/list)
(require Q/Lib/struct)
(require Q/Lib/contracts)
(require Q/Common/interfaces/serializable)

(provide
 posn?
 posn
 hash->posn
 posn-row
 posn-column
 directions
 axes
 vertical-axis
 horizontal-axis
 direction-names
 direction-name?
 axis?)

(provide/cond-contract
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
  (-> (listof posn?) boolean?)]
 [axis-of
  (->i ([pns (listof posn?)])
       #:pre/name (pns)
       "must be more than one posn in the list"
       (>= (length pns) 2)
       #:pre/name (pns)
       "posns must be aligned on an axis"
       (same-axis? pns)
       [result axis?])]
 [posn<?
  (-> posn? posn? boolean?)]
 [sort-posns
  (-> (listof posn?) (listof posn?))])

#; {type Posn = (posn Integer Integer)}
;; A Posn is a (posn r c), which represent a row and column.
(struct posn (row column)
          #:transparent
          #:methods gen:serializable
          [(define (->jsexpr p)
             (match-define [posn row column] p)
             (hash 'row row 'column column))])

#; {JCoord -> Posn}
(define (hash->posn jcoord)
  (define row (hash-ref jcoord 'row))
  (define col (hash-ref jcoord 'column))
  (posn row col))


#; {Posn Posn -> Boolean}
;; Is the first posn smaller than the second in row-column order?
(define (posn<? p1 p2)
  (or (< (posn-row p1) (posn-row p2))
      (and (= (posn-row p1) (posn-row p2))
           (< (posn-column p1) (posn-column p2)))))

#; {[Listof Posn] -> [Listof Posn]}
;; Sort the given list of posns using row-column order.
(define (sort-posns posns)
  (sort posns
        posn<?))

#; {type Direction = (U 'up 'down 'left 'right)}
;; A Direction represents a pair of '(Δr . Δc) that are unit vector translations in one of the
;; four distinct directions in a square grid.
;; A Direction thus has two axes, vertical and horizontal. A Direction is a translation in only one
;; axis. For the vertical axis, up is negative and down is positive. For the horizontal axes, left
;; is negative and right is positive.

#; {type Axis = (U '(up down) '(left right))}
;; An Axis is a 2-list of distinct directions (list α β) such that, for any position φ,
;; (posn-translate (posn-translate φ α) β) = φ

(define vertical-directions '((up . (-1 . 0)) (down . (1 . 0))))
(define vertical-axis (map car vertical-directions))

(define horizontal-directions '((left . (0 . -1)) (right . (0 . 1))))
(define horizontal-axis (map car horizontal-directions))

(define directions
  (append vertical-directions horizontal-directions))
(define axes
  (list vertical-axis horizontal-axis))

(define direction-names (map car directions))

#; {Any -> Boolean}
(define (direction-name? a)
  (member? a direction-names))

#; {Any -> Boolean}
(define (axis? a)
  (and (list? a)
       (= 2 (length a))
       (ormap (curry equal? a)
              (list vertical-axis horizontal-axis))))

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
  (match-define [cons dr dc] (cdr (assoc dir directions)))
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

#; {[Listof Posn] -> Axis}
;; Retrieve the axis the posns align on
(define (axis-of posns)
  (cond [(posns-same-row? posns)    horizontal-axis]
        [(posns-same-column? posns) vertical-axis]))
