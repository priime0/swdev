#lang racket

(require struct-plus-plus)
(require threading)

(require Q/Common/map)
(require Q/Common/config)
(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/util/list)

#; {type PlayerId = Symbol}
;; A PlayerId represents a _unique_ identifier for a player during a game.

#; {Any -> Boolean}
(define player-id? symbol?)

#; {type TurnAction = (U [List 'place-tile [Listof TilePlacement]]
                         [List 'exchange]
                         [List 'pass])}
;; A TurnAction represents a possible action during a player turn, and is one of:
;; - A placement of tiles onto the board at the corresponding locations in the given order
;; - An exchange of all tiles in a player's hand
;; - Skipping a player's turn (withdrawing from performing any actions)

#; {Any -> Boolean}
(define (turn-action? a)
  (match a
    [(list 'place-tile placements)
     (andmap tile-placement? placements)]
    [(or '(exchange) '(pass)) #t]
    [_                        #f]))

#; {type TurnInfo = (turn-info PlayerId
                               [Listof Tile]
                               [HashTable PlayerId Natural]
                               Board)};
;; A TurnInfo represents all the data necessary for a player to take their turn, containing their
;; player id, their tiles, the points of every player, the history of moves made, and the current
;; board state.
(struct++ turn-info
          ([player-id    player-id?]
           [player-tiles (listof tile?)]
           [scores       (hash/c string? natural?)]
           [history      (listof (cons/c player-id? turn-action?))]
           [board        board?])
          #:transparent)

#; {type TilePlacement = [Pair Posn Tile]}
;; A TilePlacement represents a request from the player to place the given tile at the given
;; position.

#; {Any -> Boolean}
(define (tile-placement? a)
  (and (pair? a)
       (posn? (car a))
       (tile? (cdr a))))



#; {type PlayerState = (player-state Natural [Listof Tile])}
;; A PlayerState represents a participating player's state during any instant of time, containing
;; the points the player accrued during the game, along with the tiles in their hand during a move.
;; INVARIANT: The hand of a player state has a length L such that 0 ≤ L ≤ (*hand-size*).
(struct++ player-state
          ([score  natural?]
           [hand   (listof tile?)])
          #:transparent)


#; {type GameState = (game-state Board
                                 [Dequeof Tile]
                                 [HashTable PlayerId PlayerState]
                                 [Listof [Pairof PlayerId TurnAction]]
                                 [Dequeof PlayerId])}
;; A GameState represents the state of a game at any instant of time, containing the board's current
;; state at that instant, along with the list of remaining tiles, state of participating players,
;; history of moves made in order of recency, and their turn orders.
;; INVARIANT: A player β is in the `players` hashtable iff β is in the `turn-queue` deque. That is,
;; there is a bijection between (hash-keys `players`) and `turn-queue`.
(struct++ game-state
          ([board         board?]
           [tiles         (listof tile?)]
           [player-states (hash/c player-id? player-state?)]
           [history       (listof turn-action?)]
           [turn-queue    (listof symbol?)])
          #:transparent)

#; {Tile [Listof Tile] [Listof PlayerId] -> GameState}
;; Creates a fresh game state with the given referee tile, set of tiles (shuffling them), and the
;; player IDs.
;; ASSUME: the players are sorted by age in non-increasing order.
;; ASSUME: the `start-tile` and `tiles` comprise all the tiles in the game.
(define (make-game-state start-tile tiles player-ids)
  (define-values (states^^ tiles^^)
    (for/fold ([states^ (hash)]
               [tiles^ (shuffle tiles)])
              ([player-id player-ids])
      (define default-score 0)
      (define-values (hand tiles+) (split-at tiles^ (*hand-size*)))
      (define state (player-state default-score hand))
      (define states+ (hash-set states^ player-id state))
      (values states+ tiles+)))
  (game-state++ #:board (make-board start-tile)
                #:tiles tiles^^
                #:player-states states^^
                #:history '()
                #:turn-queue player-ids))


#; {[HashTable PlayerId PlayerState] -> [HashTable PlayerId Natural]}
;; Extract the score of each player from the given player states map.
(define (player-states->player-scores states)
  (define (extract-score name state)
    (values name (player-state-score state)))
  (hash-map/copy states extract-score))


#; {GameState -> TurnInfo}
;; Produce the TurnInfo for the current player to make a decision from, using the given game state.
(define (game-state->turn-info gs)
  (match-define [game-state board _ states history turn-queue] gs)

  (define player-id (first turn-queue))
  (define state (hash-ref states player-id))
  (define player-tiles (player-state-hand state))

  (define scores (player-states->player-scores states))

  (turn-info player-id player-tiles scores history board))

#; {GameState PlayerId -> GameState}
;; Kick the player from the current game state.
;; EXCEPT: When the given player id is not in the game.
(define (kick-player gs p-id)
  (match-define [game-state board tiles states history turn-queue] gs)

  (unless (member? p-id turn-queue)
    (error 'kick-player
           "player ~a does not exist!"
           p-id))

  (define hand (player-state-hand (hash-ref states p-id)))

  (define tiles+      (append tiles hand))
  (define states+     (hash-remove states p-id))
  (define turn-queue+ (remf (curry string=? p-id) turn-queue))

  (game-state board tiles+ states+ turn-queue+))

#; {PlayerState [Listof Tile] -> PlayerState}
;; Removes the given tiles from the hand of the given player state.
(define (remove-from-hand state tiles)
  (define hand (player-state-hand state))
  (define hand+ (remove-from tiles hand))
  (set-player-state-hand state hand+))

#; {PlayerState -> (values PlayerState [Listof Tile])}
;; Empties the hand of the given player state, returning the new empty-handed player state,
;; and its former tiles.
(define (clear-hand state)
  (define hand (player-state-hand state))
  (define state+ (set-player-state-hand state '()))
  (values state+ hand))

#; {PlayerState [Listof Tiles] -> (values PlayerState [Listof Tiles])}
;; Repenlishes the hand of the given player state from the given list of tiles, returning
;; the new player state and the remaining tiles.
(define (refill-hand state tiles)
  (define hand        (player-state-hand state))
  (define hand-length (length hand))
  (define missing     (- (*hand-size*) hand-length))
  (define available   (length tiles))

  (define-values (new-tiles tiles+)
    (split-at tiles
              (min missing available)))
  (define hand+ (append hand new-tiles))

  (define state+ (set-player-state-hand state hand+))
  (values state+ tiles+))

#; {GameState TurnAction -> GameState}
;; Performs the given turn action for the current player in this game state, and updates the turn
;; queue.
;; ASSUME: the given game state has a non-zero amount of players.
(define (take-turn gs action)
  (define gs+
    (match action
      [(list 'place-tile placements) (take-turn/placement gs placements)]
      [(list 'exchange)              (take-turn/exchange gs)]
      [(list 'pass)                  (identity gs)]))
  (end-turn gs+))

#; {Board [Listof TilePlacement] -> Board}
;; Constructs a new board with all of the given tile placements.
;; ASSUME each tile placement is valid.
(define (place-tiles board placements)
  (for/fold ([board^ board])
            ([placement placements])
    (match-define [cons posn tile] placement)
    (add-tile board^ posn tile)))
          
#; {GameState [Listof TilePlacement] -> GameState}
;; Places the given list of placements, updating the board, player hand, and remaining tiles.
(define (take-turn/placement gs placements)
  (match-define [game-state board tiles states history turn-queue] gs)
  (define player       (first turn-queue))
  (define state        (hash-ref states player))

  (define board+       (place-tiles board placements))

  (define placed-tiles (map cdr placements))
  (define state+       (remove-from-hand state placed-tiles))
  (define-values       (state++ tiles+) (refill-hand state+ tiles))
  (define states+      (hash-set states player state++))

  (game-state board+ tiles+ states+ history turn-queue))

#; {GameState -> GameState}
;; Exchange the current player's tiles for new ones from the game state's tiles.
;; 
(define (take-turn/exchange gs)
  (match-define [game-state board tiles states history turn-queue] gs)
  (define player (first turn-queue))
  (define state  (hash-ref states player))
  (define-values (state+ hand) (clear-hand state))

  (define-values (state++ tiles+) (refill-hand state+ tiles))
  (define tiles++ (append tiles+ hand))
  (define states+ (hash-set states player state++))

  (game-state board tiles++ states+ history turn-queue))

#; {GameState -> GameState}
;; Updates the turn queue, moving the current player to the end.
(define (end-turn gs)
  (define turn-queue (game-state-turn-queue gs))
  (set-game-state-turn-queue gs (rotate-left-1 turn-queue)))

(module+ test
  (define tilecart (cartesian-product tile-colors tile-shapes))
  (define tile-set (map (curry apply tile) tilecart))
  (define all-tiles (make-list 10 tile-set))
  (define all-tiles+ (flatten all-tiles))
  (define gs (make-game-state (tile 'red 'square)
                              all-tiles+
                              (list 'lucas 'andrey)))
  (define turn-info-1 (game-state->turn-info gs)))
