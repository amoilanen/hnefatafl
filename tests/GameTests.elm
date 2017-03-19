module GameTests exposing (all)

import Game exposing(isValidMove, Position, GameState)
import Test exposing (..)
import Expect exposing (Expectation, equal, true, false)


isTrue: Bool -> Expectation
isTrue =
  Expect.true ""

isFalse: Bool -> Expectation
isFalse =
  Expect.false ""

-- Simplified board 5 x 5: x - escape cells, d, a - pieces, k - king
--x _ a _ x
--_ _ d _ _
--a d k d a
--_ _ d _ _
--x a _ _ x

attackers : List Position
attackers =
  [ (Position 0 2)
  , (Position 2 0)
  , (Position 1 4)
  , (Position 4 2)
  ]

defenders : List Position
defenders =
  [ (Position 1 2)
  , (Position 2 1)
  , (Position 2 3)
  , (Position 3 2)
  ]

king : Position
king =
  Position 2 2

-- Leftmost attacker piece is selected
selectedPiece : Maybe Position
selectedPiece =
  Just (Position 0 2)

boardSize : Int
boardSize = 5

state : GameState
state =
  GameState attackers defenders king selectedPiece boardSize

all : Test
all =
  describe "Game"
    [ describe "isValidMove"
      [ test "can move to empty cell" <|
        \() ->
          isTrue (isValidMove state (Position 0 1))
      , test "cannot move to empty cell in different column and row" <|
        \() ->
          isFalse (isValidMove state (Position 1 1))
      , test "cannot move outside of the board" <|
        \() ->
          isFalse (isValidMove state (Position 0 6))
      , test "cannot move to a position where another piece is" <|
        \() ->
          let
            updatedState = { state | pieceSelected = Just (Position 1 4) }
          in
            isFalse (isValidMove updatedState (Position 1 2))
      ]
    ]