module Tests exposing (..)

import Test exposing (..)
import UtilTests

all : Test
all =
    describe "All"
        [ UtilTests.all
        ]
