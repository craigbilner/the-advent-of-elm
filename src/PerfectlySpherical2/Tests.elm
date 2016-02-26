module PerfectlySpherical2.Tests where

import ElmTest exposing (..)
import String


import PerfectlySpherical2 as PS2

all : Test
all =
    suite "PerfectlySpherical2 should"
        [
            test "return 3 for ^v" (assertEqual 3 (PS2.run "^v")),
            test "return 3 for ^>v<" (assertEqual 3 (PS2.run "^>v<")),
            test "return 11 for ^v^v^v^v^v" (assertEqual 11 (PS2.run "^v^v^v^v^v"))
        ]
