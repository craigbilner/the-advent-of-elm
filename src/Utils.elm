module Utils where

import String
import Regex


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

