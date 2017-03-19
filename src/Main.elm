import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (range, map)
import Util exposing(updateList)

import Game exposing(Position, GameState, isValidMove)

--TODO: Pieces cannot be moved out of the board
--TODO: Pieces cannot be moved to cells where other pieces
--TODO: Pieces cannot be moved through other pieces
--TODO: Pieces cannot be moved to escape cells except for the king piece

--TODO: Pieces are moved only in turns (first player, then second player)
--TODO: If other piece is between two pieces of other type it can be taken
--TODO: When first player moves between two pieces of the opposite player, first player piece cannot be taken
--TODO: ... Other rules

--TODO: Winning condition checking: king escapes to the escape cells, or king is taken, or moves are repeating 3 times
--TOOD: Resetting the game
--TODO: Re-factor: extract modules
--TODO: Computer player
--TODO: Choosing the side and the first move
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL

boardSize : Int
boardSize = 10

-- TODO: Re-factor repetition when generating data
attackersInitially : List Position
attackersInitially =
  [ (Position 0 3)
  , (Position 0 4)
  , (Position 0 5)
  , (Position 0 6)
  , (Position 0 7)
  , (Position 1 5)
  , (Position 3 0)
  , (Position 4 0)
  , (Position 5 0)
  , (Position 6 0)
  , (Position 7 0)
  , (Position 5 1)
  , (Position 10 3)
  , (Position 10 4)
  , (Position 10 5)
  , (Position 10 6)
  , (Position 10 7)
  , (Position 9 5)
  , (Position 3 10)
  , (Position 4 10)
  , (Position 5 10)
  , (Position 6 10)
  , (Position 7 10)
  , (Position 5 9)
  ]

defendersInitially : List Position
defendersInitially =
  [ (Position 4 4)
  , (Position 4 5)
  , (Position 4 6)
  , (Position 3 5)
  , (Position 5 3)
  , (Position 5 4)
  , (Position 5 6)
  , (Position 5 7)
  , (Position 6 4)
  , (Position 6 5)
  , (Position 6 6)
  , (Position 7 5)
  ]

kingInitially : Position
kingInitially =
  Position 5 5

model : GameState
model =
  GameState attackersInitially defendersInitially kingInitially Nothing boardSize

isEscapePosition : Position -> Bool
isEscapePosition {row, column} =
  let
    isCorner = (row == 0 || row == boardSize) && (column == 0 || column == boardSize)
    isCenter = (row == (boardSize // 2) && column == (boardSize // 2))
  in isCorner || isCenter

-- UPDATE
type Msg
  = Change String
  | PieceSelected Position
  | MovePieceTo Position

update : Msg -> GameState -> GameState
update msg model =
  case msg of
    Change newContent ->
      model
    PieceSelected position ->
      let
        newPosition = case model.pieceSelected of
          Just previousPosition ->
            if position == previousPosition then
              Nothing
            else
              Just position
          Nothing -> Just position
      in { model | pieceSelected = newPosition }
    MovePieceTo position ->
      if isValidMove model position then
        case model.pieceSelected of
          Just pieceSelected ->
            let
              attackers =
                updateList
                  model.attackers
                  (\attacker -> attacker == pieceSelected)
                  position
              defenders =
                updateList
                  model.defenders
                  (\defender -> defender == pieceSelected)
                  position
              king =
                if model.king == pieceSelected then
                  position
                else
                  model.king
            in GameState attackers defenders king Nothing model.boardSize
          Nothing ->
            model
      else
        model

-- VIEW
view : GameState -> Html Msg
view model =
  div [class "board"] (map (\row ->
    div [class "row"] (map (\column -> cell model row column) (range 0 boardSize))
  ) (range 0 boardSize))

cell : GameState -> Int -> Int -> Html Msg
cell model row column =
  let
    position = Position row column
    selectedClass = case model.pieceSelected of
      Just selectedPosition ->
        if selectedPosition == position then
          "selected "
        else
          ""
      Nothing ->
        ""
    isPotentialMoveDestination = isValidMove model position
    escapeCellClass =
      if isEscapePosition position then
        "escape-cell "
      else
        ""
    cellClass = "cell " ++ escapeCellClass ++ selectedClass

    pieceChildren =
      if List.member position model.attackers then
        [div [class "attacker", onClick (PieceSelected position)] []]
      else if List.member position model.defenders then
        [div [class "defender", onClick (PieceSelected position)] []]
      else if position == model.king then
        [div [class "king", onClick (PieceSelected position)] []]
      else
        []
    moveTargetChildren =
      if isPotentialMoveDestination then
        [div [class "potential-move-destination"] []]
      else
        []
    cellChildren = pieceChildren ++ moveTargetChildren
    cellAttributes =
      if List.isEmpty pieceChildren then
        [class cellClass, onClick (MovePieceTo position)]
      else
        [class cellClass]
  in div cellAttributes cellChildren
