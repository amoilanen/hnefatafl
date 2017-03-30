module Game exposing(Position, GameState, isValidMove)

import List exposing (any)

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

--TODO: Extract the function for checking if the piece is in the way, simplify range checking
isValidMove : GameState -> Position -> Bool
isValidMove model toPosition =
  case model.pieceSelected of
    Just pieceSelected ->
      let
        inSameRowOrColumn = pieceSelected.row == toPosition.row || pieceSelected.column == toPosition.column
        stillOnBoard = toPosition.row >= 0 && toPosition.row <= model.boardSize
          && toPosition.column >= 0 && toPosition.column <= model.boardSize
        allPieces = model.attackers ++ model.defenders ++ [model.king]
        moveRangeRow = List.range
          (min toPosition.row pieceSelected.row)
          (max toPosition.row pieceSelected.row)
        moveRangeColumn = List.range
          (min toPosition.column pieceSelected.column)
          (max toPosition.column pieceSelected.column)
        piecesInMoveRange = List.foldl
          (\piece acc ->
            if List.member piece.row moveRangeRow && List.member piece.column moveRangeColumn then
              acc + 1
            else
              acc
          )
          0 allPieces
        isAnotherPieceInTheWay = piecesInMoveRange > 1
      in inSameRowOrColumn && stillOnBoard && not isAnotherPieceInTheWay
    Nothing ->
      False