#lang racket

(require racket/class)

(require Q/Common/interfaces/playable)

(provide mock-player%)

#; {class MockPlayer}
;; A MockPlayer is player that accumulates method calls and forwards
;; them to the delegate player it is initialized with.
(define mock-player%
  (class* object% (playable<%>)
    (super-new)
    (init-field player-delegate)
    ;; [Listof [List Symbol Any ...]]
    ;; method calls in most-recent -> oldest order
    (init-field [call-log '()])

    (define/public (name)
      (set! call-log (cons `(name) call-log))
      (send player-delegate name))

    (define/public (setup pub-state hand)
      (set! call-log (cons `(setup ,pub-state ,hand) call-log))
      (send player-delegate setup pub-state hand))

    (define/public (take-turn pub-state)
      (set! call-log (cons `(take-turn ,pub-state) call-log))
      (send player-delegate take-turn pub-state))

    (define/public (new-tiles hand)
      (set! call-log (cons `(new-tiles ,hand) call-log))
      (send player-delegate new-tiles hand))

    (define/public (win won?)
      (set! call-log (cons `(win ,won?) call-log))
      (send player-delegate win won?))

    #; {-> [Listof [List Symbol Any ...]]}
    ;; Returns the call log of this mock player.
    (define/public (get-log)
      (reverse call-log))))
