import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class)
import List exposing (range, map)

--TODO: Possible to move pieces
--TODO: Only moves horizontally or vertically are possible
--TODO: Add first unit tests for some of the functions that encode the logic of the game
--TODO: Pieces cannot be moved out of the board
--TODO: Pieces cannot be moved to cells where other pieces
--TODO: Pieces cannot be moved through other pieces
--TODO: Pieces cannot be moved to escape cells except for the king piece
--TODO: If other piece is between two pieces of other type it can be taken
--TODO: ... Other rules
--TODO: Winning condition checking
--TOOD: Resetting the game
--TODO: Re-factor: extract modules
--TODO: Computer player
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL
type alias Position =
  { row : Int
  , column : Int
  }

type alias Model =
  { attackers : List Position
  , defenders : List Position
  , king : Position
  }

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

model : Model
model =
  Model attackersInitially defendersInitially kingInitially

-- UPDATE
type Msg
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      model

-- VIEW
view : Model -> Html Msg
view model =
  div [class "board"] (map (\row ->
    div [class "row"] (map (\column -> cell model row column) (range 0 10))
  ) (range 0 10))

--TODO: Display escape cells and the central cell differently from other cells
cell : Model -> Int -> Int -> Html Msg
cell model row column =
  let
    position = Position row column
    cellChildren =
      if List.member position model.attackers then
        [div [class "attacker"] []]
      else if List.member position model.defenders then
        [div [class "defender"] []]
      else if position == model.king then
        [div [class "king"] []]
      else
        []
  in div [class "cell"] cellChildren
