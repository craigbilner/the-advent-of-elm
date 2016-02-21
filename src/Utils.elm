module Src.Utils where

import String

safeToInt : String -> Int
safeToInt =
        String.toInt >> Result.toMaybe >> Maybe.withDefault 0

safeListMin : List Int -> Int
safeListMin =
        List.minimum >> Maybe.withDefault 0

double : Int -> Int
double a =
        a * 2
