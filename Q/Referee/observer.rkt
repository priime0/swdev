#lang racket

(require Q/Common/game-state)
(require Q/Common/config)

(require racket/class)
(require racket/set)
(require data/gvector)

(require 2htdp/universe)
(require 2htdp/image)

(provide
 observer<%>
 default-observer%
 observer-manager%)


;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {interface Observer}
;; An Observer represents a component that facilitates spectator interaction with a game as it
;; progresses.
(define observer<%>
  (interface ()
    #; {Observer PrivateState -> Void}
    ;; Receive the next private game state to observe.
    [observe (->m priv-state/c void?)]

    #; {Observer -> Void}
    ;; Signal that the game has ended.
    [terminate (->m void?)]))


;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {class ObserverManager}
;; A ObserverManager is a concrete implementation the Observer interface that represents a
;; collection of Observers to dispatch received messages to.
(define observer-manager%
  (class* object% (observer<%>)
    (super-new)
    (field [observers (mutable-set)])

    (define/public (observe priv-state)
      (for ([observer observers])
        (send observer observe priv-state)))

    (define/public (terminate)
      (void))

    (define/public (connect observer)
      (unless (set-member? observers observer)
        (set-add! observers observer)))

    (define/public (disconnect observer)
      (when (set-member? observers observer)
        (set-remove! observers observer)))))


#; {class DefaultObserver}
;; A DefaultObserver represents a concrete implementation of the Observer interface which collects
;; a list of game states and displays them in an interactive program when the game has ended.
(define default-observer%
  (class* object% (observer<%>)
    (super-new)
    (field [states (make-gvector)])


    (define/public (observe priv-state)
      (when (zero? (gvector-count states))
        (send this start-observation))
      (gvector-add! states priv-state))

    (define/public (terminate)
      (void))

    (define/public (start-observation)
      (struct world-state (states index path end?) #:transparent)
      (thread
       (thunk
        (define ws0 (world-state states 0 "" #f))

        (big-bang ws0
          [stop-when world-state-end?]
          [to-draw
           (lambda (ws)
             (match-define [world-state states index path _] ws)
             (define state (gvector-ref states index))
             (define rendered-state (render-game-state state))
             (define rendered-state/width (image-width rendered-state))
             (define rendered-state/height (image-height rendered-state))
             (define textbox (frame (text path (/ (*game-size*) 2) 'black)))
             (define scene-width (max 1080 rendered-state/width))
             (define scene-height (max 600 rendered-state/height))
             (define scene (empty-scene scene-width scene-height))
             (define scene+ (place-image rendered-state 540 300 scene))
             (place-image textbox 540 550 scene+))]
          [on-key
           (lambda (ws key)
             (match-define [world-state states index path _] ws)
             (define states-len (gvector-count states))

             (case key
               [("left")
                (cond [(zero? index) ws]
                      [else (world-state states (sub1 index) path #f)])]
               [("right")
                (cond [(= (add1 index) states-len) ws]
                      [else (world-state states (add1 index) path #f)])]
               [("escape") (world-state states index path #t)]
               [("\b")
                (define path+
                  (if (non-empty-string? path)
                      (substring path 0 (sub1 (string-length path)))
                      path))
                (world-state states index path+ #f)]
               [("up"
                 "down"
                 "start"
                 "cancel"
                 "clear"
                 "shift"
                 "rshift"
                 "control"
                 "rcontrol"
                 "print"
                 "insert"
                 "wheel-up"
                 "wheel-down"
                 "wheel-left"
                 "wheel-right")
                ws]
               [else
                (world-state states index (string-append path key) #f)]))]))))))
