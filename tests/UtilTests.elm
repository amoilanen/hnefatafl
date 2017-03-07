module UtilTests exposing (..)

import Test exposing (..)
import Expect exposing (equal)
import Util exposing (updateList)

all : Test
all =
  describe "Util"
    [ describe "updateList"
      [ test "has item to update" <|
        \() ->
          equal [1, 4, 3] (updateList [1, 2, 3] (\x -> x == 2) 4)
      , test "no item to update" <|
        \() ->
          equal [1, 2, 3] (updateList [1, 2, 3] (\x -> x < 0) 4)
      , test "all items are updated" <|
        \() ->
          equal [4, 4, 4] (updateList [1, 2, 3] (\x -> x > 0) 4)
      ]
    ]