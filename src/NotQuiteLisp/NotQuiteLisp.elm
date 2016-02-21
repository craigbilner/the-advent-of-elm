module Src.NotQuiteLisp.NotQuiteLisp where

import String

convertBracketToNumber: String -> Int
convertBracketToNumber bracket = 
        case bracket of
                "(" ->
                        1
                ")" ->
                        -1
                _ ->
                        0

run: String -> Int
run bracketInput =
        String.split "" bracketInput
          |> List.map convertBracketToNumber
          |> List.sum
