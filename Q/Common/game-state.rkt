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

(require Q/Common/map)
(require Q/Common/player)
(require Q/Common/turn-info)
(require Q/Common/config)
(require Q/Common/data/turn-action)
(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/data/event)
(require Q/Common/util/list)
(require Q/Common/util/image)
(require Q/Common/util/misc)
(require Q/Common/interfaces/serializable)

(provide
 player-id?
 turn-action?
 (struct-out game-state)
 hash->turn-info++
 (contract-out
  [make-game-state
   (->i ([tiles (listof tile?)] [players (listof player-id?)])
        #:pre/name (tiles players)
        "not enough tiles!"
        (< (length tiles)
           (add1 (* (length players)
                    (*hand-size*))))
        [result game-state?])]
  [game-state->turn-info gs->gs/c]
  [take-turn
   (->i ([gs game-state?] [t turn-action?])
        #:pre/name (gs)
        "game is over!"
        (not (game-over? gs))
        [result turn-info?])]
  [remove-player gs->gs/c]))

;; 1) End of game? (export game-over?)  ^ referee now has to be able to
;; check the round-based game-over rules (could delegate this to
;; game-state) (maybe)
;;
;; 2) Referee could store list of past
;; game states to resume from any point--or we could make this history
;;
;; 3) Create the game state with players - sort by age, then pass in to game state (make-game-state)
;;
;; 4) (def gs+ (take-turn ...)) ... (your-new-tiles (first (game-state-players gs+)))
;; Define (new-tiles ...) helper (public) for referee to inform the player who just took their turn
;; of their new hand
;;
;; 5) EOG - produce final message for each player (win/lose, final score, etc)
;;
;; we have partial (1) (3), no functionality for (2), (4), (5)



#; {JPub -> TurnInfo}
(define (hash->turn-info++ jpub)
  (define jmap (hash-ref jpub 'map))
  (define tile* (hash-ref jpub 'tile*))
  (define players (hash-ref jpub 'players))
  (define jplayer (first players))
  (define scores* (rest players))
  
  (define state (hash->player-state++ jplayer))
  (define board (hash->board++ jmap))
  (define scores
    (for/list ([score scores*] [i (in-naturals)])
      (cons (string->symbol (number->string i)) score)))
  
  (turn-info state scores '() board tile*))


#; {type GameState = (game-state Board
                                 [Dequeof Tile]
                                 History
                                 [Dequeof PlayerState])}
;; A GameState represents the state of a game at any instant of time, containing the board's current
;; state at that instant, along with the list of remaining tiles, history of moves made in order of
;; recency, and the player states in order of turns to take.
;; TODO: Remove history and change game-over? 
(struct++ game-state
          ([board    board?]
           [tiles    (listof tile?)]
           [history  history?]
           [players  (listof player-state?)])
          #:transparent)

#; {GameState -> Boolean}
;; Is the game over?
(define (game-over? gs)
  (member? (game-end) (game-state-history gs)))

(define gs->gs/c
  (->i ([gs game-state?])
        #:pre/name (gs)
        "game is over!"
        (not (game-over? gs))
        [result turn-info?]))

#; {GameEvent GameState -> GameState}
;; Updates the history of the given game state with the provided procedure
(define (add-to-history evt gs)
  (set-game-state-history gs (cons evt (game-state-history gs))))

#; {([Listof Tile] -> [Listof Tile]) GameState -> GameState}
;; Updates the tiles of the given game state with the provided procedure
(define (apply-tiles f gs)
  (set-game-state-tiles gs (f (game-state-tiles gs))))

#; {(Board -> Board) GameState -> GameState}
;; Updates the board of the given game state with the provided procedure
(define (apply-board f gs)
  (set-game-state-board gs (f (game-state-board gs))))

#; {([Listof PlayerState] -> [Listof PlayerState]) GameState -> GameState}
;; Updates the player state queue of the given game state with the provided procedure
(define (apply-players f gs)
  (set-game-state-players gs (f (game-state-players gs))))


#; {[Listof Tile] [Listof PlayerId] -> GameState}
;; Creates a fresh game state with the given referee tile, set of tiles (shuffling them), and the
;; player IDs.
;; ASSUME: the players are sorted by age in non-increasing order.
;; ASSUME: the `start-tiles` comprise all the tiles in the game, and there are enough tiles to place
;;         on the board and hand out to players.
(define (make-game-state start-tiles player-ids)
  (match-define [cons start-tile rest-tiles] (shuffle start-tiles))
  (define handout-size           (* (length player-ids) (*hand-size*)))
  (define-values (handout tiles) (split-at rest-tiles handout-size))

  (define players
    (~>> handout
         (segment (*hand-size*))
         (map make-player-state player-ids)))

  (game-state++ #:board (make-board start-tile)
                #:tiles tiles
                #:history '()
                #:players players))


#; {GameState -> TurnInfo}
;; Produce the TurnInfo for the current player to make a decision from, using the given game state.
(define (game-state->turn-info gs)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define player-alist (map player-state->pair others))
  (turn-info state player-alist history board (length tiles)))

#; {GameState  -> GameState}
;; Remove the current player from the game state.
(define (remove-player gs)
  (define player (first (game-state-players gs)))
  (match-define [player-state id score hand] player)

  (~>> gs
       (apply-tiles (curryr append hand))
       (add-to-history (banish id score))
       (apply-players rest)
       ((if? game-over? (curry add-to-history (game-end))))))


#; {GameState TurnAction -> GameState}
;; Performs the given turn action for the current player in this game state, and updates the turn
;; queue, or kicks the current players and reclaims their tiles if the move is invalid according to
;; the provided rule.
(define (take-turn gs action)
  (define info (game-state->turn-info gs))
  (cond [(valid-action? info action)
         (define gs+        (apply-turn gs action))
         (define turn-score (score-turn (game-state->turn-info gs+) action))
         (define gs++       (update-score gs+ turn-score))
         (end-turn gs++ action)]
        [else
         (remove-player gs)]))

#; {GameState TurnAction -> GameState}
;; Apply the turn action to the game state
(define (apply-turn gs action)
  (match action
    [(place placements)  (take-turn/placement gs placements)]
    [(exchange)          (take-turn/exchange gs)]
    [(pass)              gs]))

#; {GameState Natural -> GameState}
;; Update current player's score
(define (update-score gs points)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define state+ (add-points state points))
  (game-state board tiles history (cons state+ others)))


#; {GameState [Listof TilePlacement] -> GameState}
;; Places the given list of placements, updating the board, player hand, and remaining tiles.
(define (take-turn/placement gs placements)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define board+        (add-tiles board placements))
  (define placed-tiles  (map placement-tile placements))
  (define state+        (remove-from-hand state placed-tiles))
  (define-values (state++ tiles+)
    (refill-hand state+ tiles))

  (game-state board+ tiles+ history (cons state++ others)))


#; {GameState -> GameState}
;; Exchange the current player's tiles for new ones from the game state's tiles.
(define (take-turn/exchange gs)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define-values (state+ hand) (clear-hand state))

  (define-values (state++ tiles+) (refill-hand state+ tiles))
  (define tiles++ (append tiles+ hand))

  (game-state board tiles++ history (cons state++ others)))


#; {GameState TurnAction -> GameState}
;; Ends the current turn, moving the current player to the end and adding the given turn action to
;; the history.
(define (end-turn gs action)
  (define player (first (game-state-players gs)))
  (define id (player-state-id player))

  (~>> gs
       (add-to-history (turn id action))
       (apply-players rotate-left-1)))


#; {GameState -> Image}
(define (render-game-state gs)
  (match-define [game-state board tiles history players] gs)
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
  (require Q/Common/util/test)
  (define tile-product (cartesian-product tile-colors tile-shapes))
  (define tile-set (map (curry apply tile) tile-product))
  (define players1 '(lucas andrey luke))
  (define players2 '(lucas andrey luke john))
  (define gs1 (apply/seed 0 make-game-state tile-set players1))
  (define gs2 (apply/seed 0 make-game-state tile-set players2))
  (define tile-set-seeded (apply/seed 0 shuffle tile-set))

 
  (define gs1+ (take-turn gs1 (place (list (placement (posn 1 0) (tile 'green 'clover))
                                           (placement (posn 1 1) (tile 'blue 'clover))))))
 
  (define gs1++ (take-turn gs1+ (place (list (placement (posn 1 2) (tile 'red 'clover))
                                             (placement (posn 1 -1) (tile 'green 'star))
                                             (placement (posn 1 -2) (tile 'yellow 'star))
                                             (placement (posn 1 -3) (tile 'blue 'star))
                                             (placement (posn 1 -4) (tile 'blue 'circle)))))))

(module+ test
  (test-case
   "make-game-state with 3 players"
   (match-define [game-state board tiles history players] gs1)
   (check-equal? board
                 (make-board (first tile-set-seeded))
                 "board is created with one tile")

   (check-equal? history
                 '()
                 "history starts empty")

   (check-equal? (length players)
                 (length players1)
                 "players queue has the same length contains 3 players")
   (check-equal? (map player-state-id players)
                 players1
                 "players queue contains all players in the order given")
   (check-equal? (map player-state-score players)
                 (make-list (length players1) 0)
                 "players start with 0 points")
   (check-equal? (map (compose length player-state-hand) players)
                 (make-list (length players1) (*hand-size*))
                 "players start with *hand-size* tiles in hand")


   (define all-player-tiles (map player-state-hand players))
   (define board-tile       (placement-tile (first (collect-sequence board (posn 0 0) vertical-axis))))
   (define all-gs-tiles     (append (flatten all-player-tiles)
                                    tiles
                                    (list board-tile)))
   (check same-elements?
          all-gs-tiles
          tile-set-seeded
          "all game state tiles is the same set as all tiles passed into game state"))

  (test-case
   "make-game-state with 4 players"
   (match-define [game-state board tiles history players] gs2)
   (check-equal? board
                 (make-board (first tile-set-seeded))
                 "board is created with one tile")

   (check-equal? history
                 '()
                 "history starts empty")

   (check-equal? (length players)
                 (length players2)
                 "players queue has the same length contains 3 players")
   (check-equal? (map player-state-id players)
                 players2
                 "players queue contains all players in the order given")
   (check-equal? (map player-state-score players)
                 (make-list (length players2) 0)
                 "players start with 0 points")
   (check-equal? (map (compose length player-state-hand) players)
                 (make-list (length players2) (*hand-size*))
                 "players start with *hand-size* tiles in hand")


   (define all-player-tiles (map player-state-hand players))
   (define board-tile       (placement-tile (first (collect-sequence board (posn 0 0) vertical-axis))))
   (define all-gs-tiles     (append (flatten all-player-tiles)
                                    tiles
                                    (list board-tile)))
   (check same-elements?
          all-gs-tiles
          tile-set-seeded
          "all game state tiles is the same set as all tiles passed into game state"))

  (test-case
   "game state -> turn info"
   (match-define [game-state board tiles history [cons curr-player others]] gs1)
   (check-equal? (game-state->turn-info gs1)
                 (turn-info++ #:state curr-player
                              #:scores (map player-state->pair others)
                              #:history history
                              #:board board
                              #:tiles-left (length tiles))))
  (test-case
   "remove player"
   (match-define [game-state board tiles history players] gs1)
   (define new-history (list (banish 'lucas 0)))
   (define new-tiles (append tiles (player-state-hand (first players))))
   (check-equal? (remove-player gs1)
                 (game-state board new-tiles new-history (rest players))
                 "removing lucas returns his tiles and removes him from the queue")))
