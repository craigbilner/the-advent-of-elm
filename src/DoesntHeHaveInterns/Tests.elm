module Src.DoesntHeHaveInterns.Tests where

import ElmTest exposing (..)
import String


import Src.DoesntHeHaveInterns.DoesntHeHaveInterns as DHHI

all : Test
all =
    suite "DoesntHeHaveInterns should"
        [
            test "return 1 for [ugknbfddgicrmopn]" (assertEqual 1 (DHHI.run ["ugknbfddgicrmopn"])),
            test "return 1 for [aaa]" (assertEqual 1 (DHHI.run ["aaa"])),
            test "return 0 for [jchzalrnumimnmhp]" (assertEqual 0 (DHHI.run ["jchzalrnumimnmhpi"])),
            test "return 0 for [haegwjzuvuyypxyu]" (assertEqual 0 (DHHI.run ["haegwjzuvuyypxyu"])),
            test "return 0 for [dvszwmarrgswjxmb]" (assertEqual 0 (DHHI.run ["dvszwmarrgswjxmb"]))
        ]
