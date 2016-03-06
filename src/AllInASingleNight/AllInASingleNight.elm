module AllInASingleNight where

import Dict
import Regex
import Debug
import Set

import Utils

regex : String
regex = "([a-zA-Z]+) to ([a-zA-Z]+) = ([0-9]+)"

sectionToTuple : List String -> ((String, String), Int)
sectionToTuple input =
        case input of
                start::end::distance::_ ->
                        ((start, end), Utils.safeToInt distance)

                _ ->
                        Debug.crash "Unknown section"

parseInput : String -> Dict.Dict (String, String) Int -> Dict.Dict (String, String) Int
parseInput section distances =
        let
            tuple = Regex.find Regex.All (Regex.regex regex) section
                    |> Utils.matchesToSubmatches
                    |> sectionToTuple
        in
            Dict.insert (fst tuple) (snd tuple) distances

extractPlaces : (String, String) -> Int -> List String -> List String
extractPlaces (start, end) _ places =
        start::end::places

uniquePlaces : Dict.Dict (String, String) Int -> Set.Set String
uniquePlaces distances =
        Dict.foldl extractPlaces [] distances
        |> Set.fromList

-- findShortestDistance : Dict.Dict (String, String) Int -> Int
findShortestDistance distances =
        let
            places = uniquePlaces distances
        in
            places

-- run : List String -> Int
run input =
        List.foldl parseInput Dict.empty input
        |> findShortestDistance
