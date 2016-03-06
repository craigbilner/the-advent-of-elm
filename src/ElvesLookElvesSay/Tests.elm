module ElvesLookElvesSay.Tests where

import ElmTest exposing (..)
import String


import ElvesLookElvesSay as ELES

all : Test
all =
    suite "ElvesLookELvesSay should"
        [ test "return 2 for 1 with 1 iteration" (assertEqual 2 (ELES.run 1 1))
        , test "return 2 for 1 with 2 iterations" (assertEqual 2 (ELES.run 2 1))        
        , test "return 4 for 1 with 3 iterations" (assertEqual 4 (ELES.run 3 1))        
        , test "return 6 for 1 with 4 iterations" (assertEqual 6 (ELES.run 4 1))        
        , test "return 6 for 1 with 5 iterations" (assertEqual 6 (ELES.run 5 1))        
        ]
