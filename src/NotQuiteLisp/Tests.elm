module NotQuiteLisp.Tests where

import ElmTest exposing (..)
import String


import NotQuiteLisp as NQL

all : Test
all =
    suite "NotQuiteLisp should"
        [
            test "return 0 for (())" (assertEqual 0 (NQL.run "(())")),
            test "return 0 for ()()" (assertEqual 0 (NQL.run "()()")),            
            test "return 3 for (((" (assertEqual 3 (NQL.run "(((")),
            test "return 3 for (()(()(" (assertEqual 3 (NQL.run "(()(()(")),
            test "return 3 for ))(((((" (assertEqual 3 (NQL.run "))(((((")),
            test "return -1 for ())" (assertEqual -1 (NQL.run "())")),
            test "return -1 for ))(" (assertEqual -1 (NQL.run "))(")),
            test "return -3 for )))" (assertEqual -3 (NQL.run ")))")),
            test "return -3 for )())())" (assertEqual -3 (NQL.run ")())())"))
        ]
