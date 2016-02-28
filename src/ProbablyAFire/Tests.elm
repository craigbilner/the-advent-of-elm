module ProbablyAFire.Tests where

import ElmTest exposing (..)
import String


import ProbablyAFire as PAF

all : Test
all =
    suite "ProbablyAFire should"
        [
            test "turn on 0,0 through 999,999" (assertEqual 1000000 (PAF.run ["turn on 0,0 through 999,999"])),
            test 
                "turn on 0,0 through 999,999 then toggle 0,0 through 999,0"
                (assertEqual
                    999000
                    (PAF.run [ "turn on 0,0 through 999,999"
                              , "toggle 0,0 through 999,0"
                              ])),
            test 
                "turn on 0,0 through 999,999 then toggle 0,0 through 999,0, then turn off 499,499 through 500,500"
                (assertEqual
                    998996
                    (PAF.run [ "turn on 0,0 through 999,999"
                              , "toggle 0,0 through 999,0"
                              , "turn off 499,499 through 500,500"
                              ]))
        ]
