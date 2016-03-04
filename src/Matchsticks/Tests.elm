module Matchsticks.Tests where

import ElmTest exposing (..)
import String


import Matchsticks as M

all : Test
all =
    suite "Matchsticks should"
        [ test "return 2 for \"\"" (assertEqual 2 (M.run [""]))
        , test "return 2 from \"abc\"" (assertEqual 2 (M.run ["abc"]))
        , test "return 3 from \"abc_\"" (assertEqual 3 (M.run ["abc_"]))
        , test "return 6 from \"abc_@\"" (assertEqual 6 (M.run ["abc_@"]))
        ]
