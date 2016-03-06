module Utils where

import String
import Regex
import Char


import ParseInt

safeToInt : String -> Int
safeToInt =
        String.toInt >> Result.toMaybe >> Maybe.withDefault 0

isInt : String -> Bool
isInt string =
        case String.toInt string of
                Ok _ ->
                        True
                _ ->
                        False

safeListMin : List Int -> Int
safeListMin =
        List.minimum >> Maybe.withDefault 0

double : Int -> Int
double a =
        a * 2

addTuples : (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) =
        ((a + c), (b + d))

isNothing : Maybe a -> Bool
isNothing maybeToTest =
        case maybeToTest of
                Nothing ->
                        True
                _ ->
                        False

removeNonEmpty : List a -> List a -> List a
removeNonEmpty listA flattened =
        if List.isEmpty listA
           then flattened
           else List.append flattened listA

flattenNonEmpties : List (List a) -> List a
flattenNonEmpties =
        List.foldl removeNonEmpty []

flattenSubmatches : Regex.Match -> List String -> List String
flattenSubmatches match submatches =
        List.map maybeStringToValue match.submatches
        |> List.filter (\a -> String.length a > 0)
        |> List.append submatches

matchesToSubmatches : List Regex.Match -> List String
matchesToSubmatches =
        List.foldl flattenSubmatches []

maybeStringToValue : Maybe String -> String
maybeStringToValue maybe =
        case maybe of
                Just a ->
                        String.trim a

                _ ->
                        ""
stringToASCII : String -> Result String String
stringToASCII input =
        let
            parsedInt = ParseInt.parseIntHex input
        in
            case parsedInt of
                    Ok num ->
                            Char.fromCode num
                            |> String.fromChar
                            |> Ok

                    Err num ->
                            Err input

factorial : Int -> Int
factorial value =
        List.product [1..value]

splitAt : Int -> List a -> (List a, List a)
splitAt pos list =
        (List.take pos list, List.drop pos list)

swapAt : Int -> List String -> List String
swapAt pos input =
        let
            split = splitAt pos input
            tailSplit = splitAt 1 (snd split)
        in
            List.append (fst split) (List.append (snd tailSplit) (fst tailSplit))

makeCombos : List String -> String -> List (List String) -> List (List String)
makeCombos input char combos =
        let
            others = List.filter (\x -> x /= char) input
        in
            List.append combos (List.map (\x -> char::x) (getAllCombos others))

getAllCombos : List String -> List (List String)
getAllCombos input =
        let
            n = List.length input
        in
            if n == 0 then
                []

            else if n == 1 then
                [input]

            else if n == 2 then
                input::(swapAt 0 input)::[]

            else
                List.foldl (makeCombos input) [] input
