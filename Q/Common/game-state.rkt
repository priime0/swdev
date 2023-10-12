#lang racket

(require (rename-in (only-in lazy define)
                    [define define/lazy]))
(require (rename-in data/functor
                    [map fmap]))

(require struct-plus-plus)
(require threading)
(require 2htdp/image)

(require Q/Common/map)
(require Q/Common/player)
(require Q/Common/config)
(require Q/Common/data/turn-action)
(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/data/event)
(require Q/Common/util/list)
(require Q/Common/util/image)

(provide
 player-id?
 turn-action?
 (struct-out turn-info)
 (struct-out game-state)
 (contract-out
  [make-game-state (-> (listof tile?) (listof player-id?) game-state?)]
  [valid-placements?
   (-> game-state? (listof (cons/c posn? tile?)) boolean?)]
  [valid-turn?
   (-> game-state? turn-action? boolean?)]
  [game-state->turn-info
   (->i ([gs game-state?])        
        #:pre/name (gs)
        "game has no players left!"
        (not (null? (game-state-players gs)))
        [result turn-info?])]
  [take-turn
   (->i ([gs game-state?] [action turn-action?])
        #:pre/name (gs action)
        "invalid action for the current player"
        (valid-turn? gs action)
        [result game-state?])]
  [remove-player
   (->i ([gs game-state?] [player player-id?])
        #:pre/name (gs player)
        "player not in game!"
        (player-in-game? gs player)
        [result game-state?])]))


#; {type TurnInfo = (turn-info PlayerState
                               [Listof [Pairof PlayerId Natural]]
                               History
                               Board)}
;; A TurnInfo represents all the data necessary for a player to take their turn, including their
;; private knowledge, along with the public knowledge of turn order, scores, history, and the board.
;; INVARIANT: `scores` contains about every other player, and is ordered in the order of turns.
(struct++ turn-info
          ([state    player-state?]
           [scores   (listof (cons/c player-id? natural?))]
           [history  history?]
           [board    board?])
          #:transparent)


#; {type GameState = (game-state Board
                                 [Dequeof Tile]
                                 History
                                 [Dequeof PlayerState])}
;; A GameState represents the state of a game at any instant of time, containing the board's current
;; state at that instant, along with the list of remaining tiles, history of moves made in order of
;; recency, and the player states in order of turns to take.
(struct++ game-state
          ([board    board?]
           [tiles    (listof tile?)]
           [history  history?]
           [players  (listof player-state?)])
          #:transparent)


#; {[Listof Tile] [Listof PlayerId] -> GameState}
;; Creates a fresh game state with the given referee tile, set of tiles (shuffling them), and the
;; player IDs.
;; ASSUME: the players are sorted by age in non-increasing order.
;; ASSUME: the `start-tiles` comprise all the tiles in the game, and there are enough tiles to place
;;         on the board and hand out to players.
;; TODO: validate above assumption in contract
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
  (turn-info state tiles history board))


#; {GameState PlayerId -> Boolean}
;; Is the given player in the current game state?
(define (player-in-game? gs player)
  (~>> gs
       game-state-players
       (map player-state-id)
       (member? player)))


#; {GameState PlayerId -> GameState}
;; Remove the player from the current game state.
(define (remove-player gs p-id)
  (match-define [game-state board tiles history players] gs)
  (define is-player?^ (curry is-player? p-id))
  (define-values (player players+)
    (find-remf is-player?^ players))
  (match-define [player-state _ score hand] player)

  (define tiles+   (append tiles hand))
  (define history+ (cons (banish p-id score) history))

  (game-state board tiles+ history+ players+))


#; {GameState TurnAction -> Boolean}
;; Is the given turn action a valid turn for the current player in the given game state?
(define (valid-turn? gs action)
  (match action
    [(place-tile placements) (valid-placements? gs placements)]
    [(exchange)              (valid-exchange? gs)]
    [(pass)                  #t]))


#; {GameState [Listof TilePlacement] -> Boolean}
;; Is the proposed placement for the current player valid in the rules of Q game?
(define (valid-placements? gs placements)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define hand         (player-state-hand state))
  (define posns        (map placement-posn placements))
  (define placed-tiles (map placement-tile placements))

  (define/lazy same-axis?
    (or (posns-same-row? posns)
        (posns-same-column? posns)))
  (define/lazy all-valid-placements?
    (andmap (curry valid-placement? board) placed-tiles posns))
  (define/lazy all-tiles-in-hand?
    (contains-all? hand placed-tiles))

  (and same-axis?
       all-valid-placements?
       all-tiles-in-hand?))


#; {GameState -> Boolean}
;; Is an exchange action valid for the current player in the rules of Q?
(define (valid-exchange? gs)
  (define tiles (game-state-tiles gs))
  (define num-available (length tiles))
  (>= num-available (*hand-size*)))


#; {GameState TurnAction -> GameState}
;; Performs the given turn action for the current player in this game state, and updates the turn
;; queue.
;; ASSUME: the given game state has a non-zero amount of players.
(define (take-turn gs action)
  (define gs+
    (match action
      [(place-tile placements) (take-turn/placement gs placements)]
      [(exchange)           (take-turn/exchange gs)]
      [(pass)               gs]))
  (end-turn gs+ action))


#; {Board [Listof TilePlacement] -> Board}
;; Constructs a new board with all of the given tile placements.
;; ASSUME each tile placement is valid.
(define (place-tiles board placements)
  (for/fold ([board^ board])
            ([pment placements])
    (match-define [placement posn tile] pment)
    (add-tile board^ posn tile)))


#; {GameState [Listof TilePlacement] -> GameState}
;; Places the given list of placements, updating the board, player hand, and remaining tiles.
(define (take-turn/placement gs placements)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define board+        (place-tiles board placements))

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
  (match-define [game-state board tiles history players] gs)
  (define id (player-state-id (first players)))
  (define history+ (cons (turn id action) history))

  (game-state board tiles history+ (rotate-left-1 players)))


#; {PlayerState -> Image}
(define (render-player-state ps)
  (match-define [player-state id score hand] ps)
  (define size (/ (*game-size*) 2))
  (define tiles-size (* (*game-size*) 2/3))
  (define text-image (text (~a id ": " score) size 'black))
  (define tiles-image
    (parameterize ([*game-size* tiles-size])
      (for/fold ([img empty-image])
                ([t (map render-tile hand)])
        (beside img t))))

  (above/align 'left text-image tiles-image))

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
  (define tile-product (cartesian-product tile-colors tile-shapes))
  (define tile-set (map (curry apply tile) tile-product))
  (define all-tiles (make-list 30 (stream->list tile-set)))
  (define all-tiles+ (flatten all-tiles))
  (define gs (make-game-state all-tiles+
                              '(lucas andrey jake josh)))
  (define turn-info-1 (game-state->turn-info gs)))

(module+ test
  (test-true
   "turn action exchange"
   (turn-action? '(exchange)))

  (test-true
   "turn action pass"
   (turn-action? '(pass)))

  (test-true
   "turn action place tile"
   (turn-action? `(place-tile ( (,(posn 1 0) . ,(tile 'red 'square))))))

  (test-false
   "not a turn action"
   (turn-action? 'lucas))

  (test-false
   "invalid turn action place tile"
   (turn-action? '(place-tile ()))))
