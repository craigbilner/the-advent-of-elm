module Src.PerfectlySpherical.Tests where

import ElmTest exposing (..)
import String


import Src.PerfectlySpherical.PerfectlySpherical as PS

all : Test
all =
    suite "PerfectlySpherical should"
        [
            test "return 2 for >" (assertEqual 2 (PS.run ">")),
            test "return 4 for ^>v<" (assertEqual 4 (PS.run "^>v<")),
            test "return 2 for ^v^v^v^v^v" (assertEqual 2 (PS.run "^v^v^v^v^v"))
        ]
