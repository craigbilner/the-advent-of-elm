module Matchsticks.Tests where

import ElmTest exposing (..)
import String


import Matchsticks as M

all : Test
all =
    suite "Matchsticks should"
        [ test "return 2 for \"\"" (assertEqual 2 (M.run ["\"\""]))
        , test "return 2 for \"abc\"" (assertEqual 2 (M.run ["\"abc\""]))
        , test "return 3 for \"aaa\\\"aaa\"" (assertEqual 3 (M.run ["\"aaa\\\"aaa\""]))
        , test "return 5 for \"\\x27\"" (assertEqual 5 (M.run ["\"\\x27\""]))
        , test "return 3 for \"something\\\\something\"" (assertEqual 3 (M.run ["\"something\\\\something\""]))
        ]
