#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(require data/gvector)
(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Common/interfaces/serializable)

(require Q/Referee/observer)

#; {class DefaultObserver}
;; A DefaultObserver represents a concrete implementation of the Observer interface which collects
;; a list of game states and displays them in an interactive program when the game has ended.
(define default-observer%
  (class* object% (observer<%>)
    (super-new)
    (field [states (make-gvector)])

    (define/public (observe priv-state)
      (define states-len (gvector-count states))
      (when (zero? states-len)
        (send this start-observation))
      (gvector-add! states priv-state)

      (define img (render-game-state priv-state))

      (define tmp-dir (simplify-path (build-path "./Tmp/")))
      (when (and (zero? states-len) (directory-exists? tmp-dir))
        (delete-directory/files tmp-dir))
      (when (not (directory-exists? tmp-dir))
        (make-directory tmp-dir))

      (define file-name (format "~a.png" states-len))
      (define img-path (simplify-path (build-path tmp-dir file-name)))

      (void))

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
             (define rendered-state (crop 0 0 900 900 (render-game-state state)))
             (define textbox (frame (text path (/ (*game-size*) 2) 'black)))
             (define scene-width 1000)
             (define scene-height 1000)
             (define scene (empty-scene scene-width scene-height))
             (define rendered-gs (above rendered-state textbox))
             (define scene+ (place-image rendered-gs (/ scene-width 2) (/ scene-height 2) scene))
             (crop 0 0 1990 1990 scene+))]
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
               [("\r")
                (cond [(non-empty-string? path)
                       (define state (gvector-ref states index))
                       (define state/json (serialize state))

                       (when (file-exists? path)
                         (delete-file path))

                       (with-output-to-file path
                         (thunk (displayln state/json)))
                       (world-state states index "" #f)]
                      [else ws])]
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
