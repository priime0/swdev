#lang racket

(require (rename-in (only-in lazy define)
                    [define define/lazy]))
(require racket/set)
(require racket/generic)

(require math/base)

(require struct-plus-plus)
(require threading)
(require 2htdp/image)
(require predicates)
(require json)

(require Q/Common/map)
(require Q/Common/player-state)
(require Q/Common/config)
(require Q/Common/turn-action)
(require Q/Common/tile)
(require Q/Common/posn)
(require Q/Lib/list)
(require Q/Lib/image)
(require Q/Lib/misc)
(require Q/Common/interfaces/serializable)
(require Q/Common/interfaces/playable)

(provide
 pub-state/c
 priv-state/c
 game-state/c
 (struct-out game-state)
 (contract-out
  [hash->pub-state
   (-> hash? pub-state/c)]
  [hash->priv-state
   (-> hash? priv-state/c)]
  [render-game-state
   (-> priv-state/c image?)]
  [make-game-state
   (->i ([tiles (listof tile?)] [players (listof (is-a?/c playable<%>))])
        #:pre/name (tiles players)
        "not enough tiles!"
        (< (add1 (* (length players)
                    (*hand-size*)))
           (length tiles))
        [result priv-state/c])]
  [priv-state->pub-state
   (-> priv-state/c pub-state/c)]
  [remove-player
   (-> priv-state/c priv-state/c)]
  [turn-valid?
   (-> game-state/c turn-action? boolean?)]
  [do-turn/action
   (-> game-state/c turn-action? game-state/c)]
  [do-turn/score
   (-> game-state/c turn-action? natural?)]
  [do-turn/rotate
   (-> priv-state/c priv-state/c)]
  [do-turn-without-rotate
   (-> priv-state/c turn-action? (values priv-state/c boolean?))]
  [new-tiles
   (-> priv-state/c (listof tile?))]))



;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {type PrivateState = (game-state Board
                                    [Dequeof Tile]
                                    [Dequeof PlayerState])}
;; A PrivateState represents the omnisicient knowledge of the state of
;; a game at any instant of time, containing the board's current state
;; at that instant, along with the list of remaining tiles, and the
;; player states in order of turns to take.

#; {Any -> Boolean}
(define (priv-state? a)
  (and (game-state? a)
       (unprotected-board/c (game-state-board a))
       ((listof tile?) (game-state-tiles a))
       ((listof player-state?) (game-state-players a))))

(define priv-state/c (flat-named-contract 'priv-state/c priv-state?))

#; {type PublicState = (game-state Board
                                   Natural                                  
                                   [List PlayerState Natural ...])}
;; A PublicState represents the public knowledge a player needs to
;; take their turn, containing the board, the number of remaining
;; tiles, and a list of the current player's private
;; state and the public states of the other players.

#; {Any -> Boolean}
(define (pub-state? a)
  (and (game-state? a)
       (unprotected-board/c (game-state-board a))
       (natural? (game-state-tiles a))
       ((cons/c player-state? (listof natural?)) (game-state-players a))))

(define pub-state/c (flat-named-contract 'pub-state/c pub-state?))

#; {type GameState = (U PrivateState
                        PublicState)}
;; A GameState is one of a PrivateState or a PublicState.
(define game-state/c (or/c pub-state/c priv-state/c))

(struct/contract game-state
                 ([board unprotected-board/c]
                  [tiles (or/c natural? (listof tile?))]
                  [players (or/c (listof player-state?)
                                 (cons/c player-state? (listof natural?)))])
                 #:transparent)


;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {[Listof Tile] [Listof Playable] -> PrivateState}
;; Creates a fresh game state with the given collection of tiles and players.
(define (make-game-state start-tiles playables)
  (match-define [cons start-tile rest-tiles] (shuffle start-tiles))
  (define handout-size           (* (length playables) (*hand-size*)))
  (define-values (handout tiles) (split-at rest-tiles handout-size))

  (define players
    (~>> handout
         (segment (*hand-size*))
         (map make-player-state _ playables)))
  
  (game-state (make-board start-tile)
              tiles
              players))

;; ----------------------------------------------------------------------------------------

#; {PrivateState -> PublicState}
;; Collects all of the public and private information for the current player.
(define (priv-state->pub-state priv-state)
  (match-define [game-state board tiles [cons state others]] priv-state)
  (define scores (map player-state-score others))
  (game-state board (length tiles) (cons state scores)))



;; ----------------------------------------------------------------------------------------


#; {PrivateState  -> PrivateState}
;; Remove the current player from the game state.
(define (remove-player priv-state)
  (match-define [game-state board tiles [cons curr-player others]] priv-state)
  (define hand   (player-state-hand curr-player))
  (define tiles+ (append tiles hand))
  (game-state board tiles+ others))


;; ----------------------------------------------------------------------------------------


#; {GameState TurnAction -> Boolean}
;; Is the given action valid for this game state?
(define (turn-valid? gs action)
  (match-define [game-state board tiles [cons curr-player others]] gs)
  (define hand  (player-state-hand curr-player))
  (define num-ref-tiles (if (list? tiles)
                            (length tiles)
                            tiles))

  (match action
    [(place pments) (valid-place-move? board hand pments)]
    [(exchange)     (valid-exchange? (length hand) num-ref-tiles)]
    [(pass)         #t]))


#; {Board [Listof Tile] [Listof TilePlacement] -> Boolean}
;; Is the given list of placements valid for this board and this hand?
(define (valid-place-move? board hand pments)
  (define aligned?
    (same-axis? (map placement-posn pments)))
  (define in-hand?
    (contains-all? hand (map placement-tile pments)))

  (and (pair? pments)
       aligned?
       in-hand?
       (valid-placements? board pments)))

#; {Natural Natural -> Boolean}
;; Is an exchange possible with a hand of this size and this number of
;; remaining tiles?
(define (valid-exchange? hand-size remaining-tiles)
  (>= remaining-tiles hand-size))


;; ----------------------------------------------------------------------------------------

#; {GameState TurnAction -> GameState}
;; Completes the turn action without scoring or advancing the turn queue or dealing new tiles.
(define (do-turn/action gs action)
  (match action
    [(place pments) (do-placements gs pments)]
    [(exchange)     (do-exchange gs)]
    [(pass)         gs]))


#; {GameState [Listof TilePlacement] -> GameState}
;; Places the tiles on the board and removes them from the current
;; player's hand, without dealing new tiles back.
(define (do-placements gs pments)
  (match-define [game-state board tiles [cons state other-players]] gs)
  (define board+        (add-tiles board pments))
  (define placed-tiles  (map placement-tile pments))
  (define state+        (remove-from-hand state placed-tiles))

  (game-state board+ tiles (cons state+ other-players)))

#; {GameState -> GameState}
;; Removes the player's current hand and adds them back to the referee's pool.
(define (do-exchange gs)
  (match-define [game-state board tiles [cons state others]] gs)
  (define hand    (player-state-hand state))
  (define state+  (clear-hand state))

  (define tiles+  (if (priv-state? gs)
                      (append tiles hand)
                      (+ tiles (length hand))))

  (game-state board tiles+ (cons state+ others)))


;; ----------------------------------------------------------------------------------------

#; {GameState TurnAction -> Boolean}
;; Does the given turn end the game?
;; ASSUME the turn has been applied with `do-turn/action` but new tiles have not yet been dealt.
(define (turn-ends-game? gs action)
  (define curr-player (first (game-state-players gs)))
  (and (place? action)
       (= (length (player-state-hand curr-player))
          0)))


;; ----------------------------------------------------------------------------------------

#; {GameState TurnAction -> Natural}
;; Scores the action for the given game state, including any end of game bonus.
;; ASSUME the action has already been applied to the given state, but tiles have not yet been dealt.
(define (do-turn/score gs action)
  (define board (game-state-board gs))
  (match action
    [(place pments) (+ (score/placements board pments)
                       (end-of-game-bonus gs action))]
    [_              0]))

#; {GameState Natural -> GameState}
;; Adds the given score, plus an optional bonus for the end of the
;; game, to the current player. 
(define (add-score gs score)
  (match-define [game-state board tiles [cons curr-player others]] gs)
  (define curr-player+ (add-points curr-player score))
  (game-state board tiles (cons curr-player+ others)))


#; {Board [Listof TilePlacement] -> Natural}
;; Scores the given placements without considering the end of game bonus.
(define (score/placements board pments)
  (define base-points   (length pments))
  (define seq-points    (score/sequences board pments))
  (define qs-points     (score/qs board pments))

  (+ base-points seq-points qs-points))

#; {Board [Listof TilePlacement] -> Natural}
;; Score the placements for the 2nd scoring rule: points added for
;; extending existing sequences
(define (score/sequences b pments)
  (~>> (get-sequences b pments)
       (filter-not (compose one? length))
       (map length)
       sum))

#; {Board [Listof TilePlacement] -> Natural}
;; Score the Qs generated by the placements on the board
(define (score/qs b pments)
  (~>> (get-sequences b pments)
       (filter q-sequence?)
       length
       (* (*points-per-q*))))

#; {GameState TurnAction -> Natural}
;; Computes if the given action incurred an end of game bonus.
(define (end-of-game-bonus priv-state action)
  (if (turn-ends-game? priv-state action)
      (*bonus*)
      0))


;; ----------------------------------------------------------------------------------------


#; {PrivateState Natural -> PrivateState}
;; Deal `n` new tiles to the current player.
(define (do-turn/deal priv-state n)
  (match-define [game-state board tiles [cons curr-player others]] priv-state)

  (define-values (hand+ tiles+) (split-at tiles n))

  (define curr-player+ (add-to-hand curr-player hand+))
  (game-state board tiles+ (cons curr-player+ others)))


#; {PrivateState TurnAction -> Natural}
;; Computes the number of tiles the referee needs to hand to the current player after they make this move.
(define (tiles-needed priv-state action)
  (match-define [game-state board tiles [cons curr-player others]] priv-state)
  (define hand  (player-state-hand curr-player))
  (match action
    [(place pments) (min (length pments) (length tiles))]
    [(exchange)     (length hand)]
    [(pass)         0]))


;; ----------------------------------------------------------------------------------------


#; {PrivateState -> PrivateState}
;; Rotates the queue left by one, moving the currently active player to the end.
(define (do-turn/rotate priv-state)
  (match-define [game-state board tiles players] priv-state)
  (define players+ (rotate-left-1 players))
  (game-state board tiles players+))


;; ----------------------------------------------------------------------------------------


#; {PrivateState TurnAction -> (values PrivateState Boolean)}
;; Completes a whole turn for the current player (without rotating the
;; queue, producing the next game state and whether this action ended
;; the game
;; PROTOCOL: A client of this code should call
;; `do-turn-without-rotate` and then `do-turn/rotate`. 
(define (do-turn-without-rotate priv-state action)
  (define priv-state+  (do-turn/action priv-state action))
  (define game-over?   (turn-ends-game? priv-state+ action))

  (define score        (do-turn/score priv-state+ action))
  (define priv-state++ (add-score priv-state+ score))

  (define new-tiles*   (tiles-needed priv-state action))
  (define priv-state+++ (do-turn/deal priv-state++ new-tiles*))

  (values priv-state+++ game-over?))


;; ----------------------------------------------------------------------------------------


#; {PrivateState -> [Listof Tile]}
;; Retrieves the entire new hand of the current player. 
(define (new-tiles priv-state)
  (define curr-player (first (game-state-players priv-state)))
  (player-state-hand curr-player))


;; ----------------------------------------------------------------------------------------

#; {JPub -> PublicState}
(define (hash->pub-state jpub)
  (define jmap (hash-ref jpub 'map))
  (define tile* (hash-ref jpub 'tile*))
  (define players (hash-ref jpub 'players))
  (define jplayer (first players))
  (define scores* (rest players))

  (define state (hash->player-state++ jplayer))
  (define board (hash->board++ jmap))
  (game-state board tile* (cons state scores*)))

;; ----------------------------------------------------------------------------------------

#; {JState -> PrivateState}
(define (hash->priv-state jpub)
  (define jmap (hash-ref jpub 'map))
  (define tile* (hash-ref jpub 'tile*))
  (define jplayers (hash-ref jpub 'players))

  (define board (hash->board++ jmap))
  (define tiles (map hash->tile++ tile*))
  (define players (map hash->player-state++ jplayers))

  (game-state board tiles players))

;; ----------------------------------------------------------------------------------------

#; {PrivateState -> Image}
(define (render-game-state gs)
  (match-define [game-state board tiles players] gs)
  (define states-image
    (for/fold ([img empty-image])
              ([ps players])
      (beside img
              (render-player-state ps)
              (empty-space 20 20))))
  (define board-image (render-board board))
  (above board-image (empty-space 20 20) states-image))

(module+ test
  (require rackunit)

  (require racket/lazy-require)

  (require racket/class)
  (require threading)

  (require Q/Lib/test)
  (require Q/Common/posn)
  (require Q/Common/tile)
  (require Q/Common/turn-action)
  (require Q/Common/map)

  (define player% (dynamic-require 'Q/Player/player 'player%))
  (define player-strategy<%> (dynamic-require 'Q/Player/strategy 'player-strategy<%>))
  (define dag% (dynamic-require 'Q/Player/dag 'dag%))
  (define ldasg% (dynamic-require 'Q/Player/ldasg 'ldasg%))
  
  (define tile-set
    (~>> (cartesian-product tile-colors tile-shapes)
         (map (curry apply tile))))

  (define dag (new dag%))
  (define ldasg (new ldasg%))

  (define luke (new player% [id 'luke] [strategy dag%]))
  (define andrey (new player% [id 'andrey] [strategy ldasg%]))
  (define lucas (new player% [id 'lucas] [strategy dag%]))

  (define players0 (list luke andrey lucas))

  (define gs1 (apply/seed 0 make-game-state tile-set players0))
  (define tile-set-seeded (apply/seed 0 shuffle tile-set))

  #;
  (define gs1+ (take-turn gs1 (place (list (placement (posn 1 0) (tile 'green 'clover))
                                           (placement (posn 1 1) (tile 'blue 'clover))))))

  #;
  (define gs1++ (take-turn gs1+ (place (list (placement (posn 1 2) (tile 'red 'clover))
                                             (placement (posn 1 -1) (tile 'green 'star))
                                             (placement (posn 1 -2) (tile 'yellow 'star))
                                             (placement (posn 1 -3) (tile 'blue 'star))
                                             (placement (posn 1 -4) (tile 'blue 'circle)))))))
