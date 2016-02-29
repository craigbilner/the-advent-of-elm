module SomeAssemblyRequired.Tests where

import ElmTest exposing (..)
import String


import SomeAssemblyRequired as SAR

input : List String
input = [ "x AND y -> d"
        , "x OR y -> e"
        , "x LSHIFT 2 -> f"
        , "y RSHIFT 2 -> g"
        , "NOT x -> h"
        , "NOT y -> i"
        , "x -> c"
        , "1 AND f -> b"
        , "123 -> x"
        , "456 -> y"
        ]

all : Test
all =
    suite "SomeAssemblyRequired  should"
        [ test "return the expected value for b" (assertEqual (Just 0) (SAR.run input "b"))
        , test "return the expected value for c" (assertEqual (Just 123) (SAR.run input "c"))
        , test "return the expected value for d" (assertEqual (Just 72) (SAR.run input "d"))
        , test "return the expected value for e" (assertEqual (Just 507) (SAR.run input "e"))
        , test "return the expected value for f" (assertEqual (Just 492) (SAR.run input "f"))
        , test "return the expected value for g" (assertEqual (Just 114) (SAR.run input "g"))
        , test "return the expected value for h" (assertEqual (Just 65412) (SAR.run input "h"))
        , test "return the expected value for i" (assertEqual (Just 65079) (SAR.run input "i"))
        , test "return the expected value for x" (assertEqual (Just 123) (SAR.run input "x"))
        , test "return the expected value for y" (assertEqual (Just 456) (SAR.run input "y"))
        ]
