#lang racket

(require (only-in 2htdp/image save-image))

(require data/gvector)
(require (only-in racket/gui put-file))
(require racket/gui/easy)
(require racket/gui/easy/operator)

(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Common/interfaces/serializable)
(require Q/Lib/path)
(require Q/Referee/observer)

(provide default-observer%)

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {class DefaultObserver}
;; A DefaultObserver represents a concrete implementation of the Observer interface which collects
;; a list of game states and displays them in an interactive program when the game has ended.
(define default-observer%
  (class* object% (observer<%>)
    (super-new)
    (field [states (make-gvector)])

    ;; Observe the given private state, starting the observation window if not already opened, and
    ;; writing any received states to the temporary directory.
    (define/public (observe priv-state)
      (define states-len (gvector-count states))

      (gvector-add! states priv-state)

      (define img (render-game-state priv-state))
      (define tmp-dir (ensure-tmp-dir! states-len))
      (write-image! img tmp-dir states-len)

      (when (zero? states-len)
        (send this start-observation tmp-dir)))

    ;; Indicate the termination of a game.
    (define/public (terminate)
      (void))

    ;; Start observation of the game, opening a native window to view and interact with.
    ;; ASSUME: that `tmp-dir` exists.
    (define/public (start-observation tmp-dir)
      (thread
       (thunk
        ;; Represents the current state to observe and graphically render.
        (define @index (@ 0))

        (define gs-panel    (make-game-state-panel states @index))
        (define image-panel (make-image-panel             @index))
        (define input-panel (make-input-panel      states @index))

        (define window-size '(1000 800))

        (define main-window
          (window gs-panel image-panel input-panel
                  #:alignment alignment/center
                  #:size      window-size))

        (render main-window)
        (void)))
      (void))))


;; ========================================================================================
;; OBSERVER FILE INTERACTIONS
;; ========================================================================================

#; {Natural -> Void}
;; Ensure that the directory exists in the current directory.
(define (ensure-tmp-dir! states-len)
  (define tmp-dir-name "./Tmp")
  (define tmp-dir (simplify-path (build-path tmp-dir-name)))
  (when (zero? states-len)
    (delete-directory-exists tmp-dir))
  (make-directory-not-exists tmp-dir)
  tmp-dir)


#; {Image Path -> Void}
;; Write/save the image on disk
(define (write-image! img tmp-dir states-len)
  (define file-name (format "~a.png" states-len))
  (define img-path (simplify-path (build-path tmp-dir file-name)))
  (save-image img img-path)
  (void))


#; {[GVectorof PrivateState] [Observableof Natural] -> Void}
;; Save the state at the given observable's index to the filepath the
;; user enters in the dialog.
(define (save states @index)
  (define state (gvector-ref states (obs-peek @index)))
  (define state/jsexpr (serialize state))
  (define file-target (put-file))
  (when file-target
    (call-with-output-file file-target
      (curry displayln state/jsexpr))))


;; ========================================================================================
;; OBSERVER COMPONENTS
;; ========================================================================================

(define alignment/center '(center center))
(define margin/center    '(10 10))

#; {[GVectorof PrivateState] [Observableof Natural] -> View}
;; Make the panel for viewing and manipulating the index representing
;; the current game state.
(define (make-game-state-panel states @index)
  ;; Go to the previous state if possible.
  (define (previous)
    (:= @index (max 0 (sub1 (obs-peek @index)))))

  ;; Go to the next state if possible.
  (define (next)
    (define states-len (gvector-count states))
    (:= @index (min (sub1 states-len)
                    (add1 (obs-peek @index)))))

  ;; Make the text label that indicates the index of the current game state.
  (define (make-index-label)
    (define @rendered-index
      (@index . ~> . (curry format "index: ~a")))
    (text @rendered-index))

  ;; game state interaction panel
  (define decrement-button (button "-" previous))
  (define increment-button (button "+" next))
  (define index-label (make-index-label))
  (hpanel decrement-button index-label increment-button
          #:alignment alignment/center
          #:margin    margin/center))


#; {[Observableof Natural] -> View}
;; Make the panel that views the current game state image.
(define (make-image-panel @index)
  (define (make-state-image @index)
    (define @image-path
      (@index . ~> . (curry format "Tmp/~a.png")))
    (image @image-path))

  (define img-container (make-state-image @index))
  (define container-size '(800 600))
  (hpanel img-container
          #:margin  margin/center
          #:style   '(hscroll vscroll)
          #:min-size container-size))


#; {[GVectorof PrivateState] [Observableof Natural] -> View}
;; Make the panel that views the current game state image.
(define (make-input-panel states @index)
  (define save-button (button "Save" (thunk (save states @index))))
  (hpanel save-button
          #:alignment alignment/center
          #:margin margin/center))


;; ========================================================================================
;; UNIT TESTS
;; ========================================================================================


(module+ test
  (require racket/class)

  (require Q/Common/tile)

  (require Q/Common/tile)
  (require Q/Player/player)
  (require Q/Player/ldasg)
  (require Q/Player/dag)
  (require Q/Player/iterative)

  (define dag (new dag%))
  (define ldasg (new ldasg%))
  (define itdag (new iterative% [strategy dag]))
  (define itldasg (new iterative% [strategy ldasg]))

  (define luke (new player% [id 'luke] [strategy itdag]))
  (define andrey (new player% [id 'andrey] [strategy itdag]))
  (define lucas (new player% [id 'lucas] [strategy itldasg]))

  (define players0 (list luke andrey lucas))

  (define gs1 (make-game-state (take start-tiles 30) players0)))
