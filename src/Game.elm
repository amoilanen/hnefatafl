module Game exposing(Position, GameState, isValidMove, isEscapePosition)

import List exposing (any)
import Util exposing(makeRange, belongsToRange)

type alias Position =
  { row : Int
  , column : Int
  }

type alias GameState =
  { attackers : List Position
  , defenders : List Position
  , king : Position
  , pieceSelected : Maybe Position
  , boardSize : Int
  }

isOnBoard : Position -> Int -> Bool
isOnBoard position boardSize =
  position.row >= 0 && position.row <= boardSize
    && position.column >= 0 && position.column <= boardSize

isEscapePosition : Position -> Int -> Bool
isEscapePosition {row, column} boardSize =
  let
    isCorner = (row == 0 || row == boardSize - 1) && (column == 0 || column == boardSize - 1)
    isCenter = (row == (boardSize // 2) && column == (boardSize // 2))
  in isCorner || isCenter

allPieces : GameState -> List Position
allPieces model = 
  model.attackers ++ model.defenders ++ [model.king]

areInSameRowOrColumn : Position -> Position -> Bool
areInSameRowOrColumn x y =
  x.row == y.row || x.column == y.column

isAnotherPieceInTheWay : Position -> Position -> List Position -> Bool
isAnotherPieceInTheWay piece toPosition otherPieces =
  let
    rowMoveRange = makeRange toPosition.row piece.row
    columnMoveRange = makeRange toPosition.column piece.column
    piecesInMoveRange = List.foldl
      (\piece acc ->
        if belongsToRange piece.row rowMoveRange && belongsToRange piece.column columnMoveRange then
          acc + 1
        else
          acc
      )
      0 otherPieces
  in
    (piecesInMoveRange > 1)

isOrdinaryPieceMovingToEscapePosition : Position -> Position -> GameState -> Bool
isOrdinaryPieceMovingToEscapePosition piece toPosition model =
  (isEscapePosition toPosition model.boardSize) && not (piece == model.king)

isValidMove : GameState -> Position -> Bool
isValidMove model toPosition =
  case model.pieceSelected of
    Just pieceSelected ->
      (areInSameRowOrColumn pieceSelected toPosition) &&
      (isOnBoard toPosition model.boardSize) &&
      not (isAnotherPieceInTheWay pieceSelected toPosition (allPieces model)) &&
      not (isOrdinaryPieceMovingToEscapePosition pieceSelected toPosition model)
    Nothing ->
      False