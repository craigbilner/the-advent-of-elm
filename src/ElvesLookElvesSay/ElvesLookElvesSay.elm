module ElvesLookElvesSay where

import String


import Utils

splitAtChange : String -> List (String, Int) -> List (String, Int)
splitAtChange char groups =
        case groups of
                [] ->
                        (char, 1)::groups

                h::t ->
                        if (fst h) == char
                           then (char, (snd h) + 1)::t
                           else (char, 1)::h::t                        

groupsToString :(String, Int) -> String -> String
groupsToString group newString =
        newString ++ (toString (snd group)) ++ (fst group)

groupByNumber : String -> String
groupByNumber =
        String.split ""
        >> List.foldr splitAtChange []
        >> List.foldl groupsToString ""

runNumbers : Int -> String -> String
runNumbers _ num =
        groupByNumber num

run : Int -> Int -> Int
run iterations input =
        List.foldl runNumbers (toString input) [1..iterations]
        |> String.length
