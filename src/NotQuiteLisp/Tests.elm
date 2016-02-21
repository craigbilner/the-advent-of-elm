module Src.NotQuiteLisp.Tests where

import ElmTest exposing (..)
import String


import Src.NotQuiteLisp.NotQuiteLisp as NotQuiteLisp

all : Test
all =
    suite "NotQuiteLisp should"
        [
            test "return 0 for (())" (assertEqual 0 (NotQuiteLisp.run "(())")),
            test "return 0 for ()()" (assertEqual 0 (NotQuiteLisp.run "()()")),            
            test "return 3 for (((" (assertEqual 3 (NotQuiteLisp.run "(((")),
            test "return 3 for (()(()(" (assertEqual 3 (NotQuiteLisp.run "(()(()(")),
            test "return 3 for ))(((((" (assertEqual 3 (NotQuiteLisp.run "))(((((")),
            test "return -1 for ())" (assertEqual -1 (NotQuiteLisp.run "())")),
            test "return -1 for ))(" (assertEqual -1 (NotQuiteLisp.run "))(")),
            test "return -3 for )))" (assertEqual -3 (NotQuiteLisp.run ")))")),
            test "return -3 for )())())" (assertEqual -3 (NotQuiteLisp.run ")())())"))
        ]
