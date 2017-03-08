module Tests exposing (..)

import Test exposing (..)
import UtilTests
import GameTests

all : Test
all =
  describe "All"
    [ UtilTests.all,
      GameTests.all
    ]