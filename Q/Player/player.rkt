#lang racket

(require racket/class)

(require Q/Common/game-state)
(require Q/Common/map)
(require Q/Common/interfaces/playable)
(require Q/Common/turn-action)
(require Q/Player/strategy-deserialize)


(provide
 player%
 exn-player%
 hash->player++)

;; This Player is a concrete, stateless implementation of Playable with a name and a Strategy to act
;; with.
(define player%
  (class* object% (playable<%>)
    (init-field id)
    (init-field strategy)
    (super-new)

    (define/public (name)
      (symbol->string id))

    (define/public (setup board tiles)
      (void))

    (define/public (take-turn pub-state)
      (unless (protected-board/c (game-state-board pub-state))
        (error 'take-turn
               "invalid board received before taking turn"))

      (send strategy choose-action pub-state))

    (define/public (new-tiles tiles)
      (void))

    (define/public (win won?)
      (void))))


;; A wrapper class over the Player that contains an additional field that indicates the expected
;; method to throw an exception.
(define exn-player%
  (class* object% (playable<%>)
    (init-field exn)
    (init-field player)
    (super-new)

    (define/public (name)
      (send player name))

    (define/public (setup board tiles)
      (if (eq? exn 'setup)
          (error 'setup "this is an expected error")
          (send player setup board tiles)))

    (define/public (take-turn pub-state)
      (if (eq? exn 'take-turn)
          (error 'take-turn "this is an expected error")
          (send player take-turn pub-state)))

    (define/public (new-tiles tiles)
      (if (eq? exn 'new-tiles)
          (error 'new-tiles "this is an expected error")
          (send player new-tiles tiles)))

    (define/public (win won?)
      (if (eq? exn 'win)
          (error 'win "this is an expected error")
          (send player win won?)))))


#; {JSExpr -> Player}
(define (hash->player++ jactor)
  (match jactor
    [(list jname jstrategy)
     (new player%
          [id (string->symbol jname)]
          [strategy (hash->strategy++ jstrategy)])]
    [(list jname jstrategy jexn)
     (define player
       (new player%
            [id (string->symbol jname)]
            [strategy (hash->strategy++ jstrategy)]))
     (new exn-player%
          [exn (string->symbol jexn)]
          [player player])]))


(module+ test
  (require rackunit)
  (require Q/Common/game-state)
  (require Q/Common/player-state)
  (require Q/Common/map)
  (require Q/Common/posn)
  (require Q/Common/tile)
  (require Q/Common/turn-action)
  (require Q/Player/dag)
  (require threading)

  (define board-1
    (~>> (make-board (tile 'orange 'diamond))
         (add-tile _ (placement (posn -1 0) (tile 'red 'diamond)))
         (add-tile _ (placement (posn -1 -1) (tile 'blue 'diamond)))))

  (define pub-state-1
    (game-state
     board-1
     17
     (list
      (player-state
       0
       (list
        (tile 'yellow 'clover)
        (tile 'green 'diamond)
        (tile 'yellow 'square)
        (tile 'red 'clover)
        (tile 'blue 'diamond)
        (tile 'purple 'circle))
       #f)
      0
      0)))

  (define pub-state-2
    (game-state
     (make-board (tile 'yellow '8star))
     6
     (list
      (player-state
       0
       (list
        (tile 'green 'clover)
        (tile 'green 'diamond)
        (tile 'green 'square)
        (tile 'red 'clover)
        (tile 'blue 'diamond)
        (tile 'purple 'circle))
       #f)
      0
      0)))

  (define pub-state-3
    (game-state
     (make-board (tile 'yellow '8star))
     5
     (list (player-state
            0
            (list
             (tile 'green 'clover)
             (tile 'green 'diamond)
             (tile 'green 'square)
             (tile 'red 'clover)
             (tile 'blue 'diamond)
             (tile 'purple 'circle))
            #f)
           0
           0)))

  (define dag (new dag%))
  (define player (new player% [id 'andrey] [strategy dag])))

(module+ test
  (test-equal?
   "choose a simple action"
   (send dag choose-action pub-state-1)
   (send player take-turn pub-state-1))

  (test-equal?
   "can't place, so exchange"
   (send dag choose-action pub-state-2)
   (send player take-turn pub-state-2))

  (test-equal?
   "can't place or exchange, so pass"
   (send dag choose-action pub-state-3)
   (send player take-turn pub-state-3)))
