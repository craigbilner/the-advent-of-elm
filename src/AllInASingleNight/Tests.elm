module AllInASingleNight.Tests where

import ElmTest exposing (..)
import String


import AllInASingleNight as AIASN

all : Test
all =
    suite "AllInASingleNight should"
        [ test "return 605 from [ London to Dublin = 464
                                , London to Belfast = 518
                                , Dublin to Belfast = 141
                                ]" (assertEqual 605 (AIASN.run [ "London to Dublin = 464"
                                                              , "London to Belfast = 518"
                                                              , "Dublin to Belfast = 141"]))
        ]
