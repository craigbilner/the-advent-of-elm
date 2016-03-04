module Matchsticks where

import String
import Char

characterLength : Char -> Int
characterLength char =
        if char == '_' then
           2

        else if char == '@' then
           4

        else
           1

countCharacters : Char -> Int -> Int
countCharacters char =
        (+)
        <| characterLength char

totalStringLength: String -> Int
totalStringLength =
        List.foldl countCharacters 2
        << String.toList

deductCharsFromString : String -> Int -> Int
deductCharsFromString word =
        (+)
        <| (-) (totalStringLength word) (String.length word)

run : List String -> Int
run =
        List.foldl deductCharsFromString 0
