module IWasToldThereWouldBeNoMath.Tests where

import ElmTest exposing (..)
import String


import IWasToldThereWouldBeNoMath as IWTTWBNM

all : Test
all =
    suite "IWasToldThereWouldBeNoMath should"
        [
            test "return 58 for [2x3x4]" (assertEqual 58 (IWTTWBNM.run ["2x3x4"])),
            test "return 43 for [1x1x10]" (assertEqual 43 (IWTTWBNM.run ["1x1x10"])),
            test "return 101 for [2x3x4, 1x1x10]" (assertEqual 101 (IWTTWBNM.run ["2x3x4", "1x1x10"]))
        ]
