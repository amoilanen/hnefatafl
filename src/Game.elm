module Game exposing(Position, GameState, isValidMove)

type alias Position =
  { row : Int
  , column : Int
  }

type alias GameState =
  { attackers : List Position
  , defenders : List Position
  , king : Position
  , pieceSelected : Maybe Position
  }

isValidMove : GameState -> Position -> Bool
isValidMove model toPosition =
  case model.pieceSelected of
    Just pieceSelected ->
      pieceSelected.row == toPosition.row || pieceSelected.column == toPosition.column
    Nothing ->
      False