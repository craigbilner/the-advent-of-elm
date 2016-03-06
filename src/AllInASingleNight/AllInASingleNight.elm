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

uniquePlaces : Dict.Dict (String, String) Int -> List String
uniquePlaces =
        Dict.foldl extractPlaces []
        >> Set.fromList
        >> Set.toList

safeGet : (String, String) -> Dict.Dict (String, String) Int -> Int
safeGet key =
        Dict.get key >> Maybe.withDefault 0

placesToDistance : Dict.Dict (String, String) Int -> List String -> Int
placesToDistance distances places =
        case places of
                [] ->
                        0

                start::finish::tail ->
                        let
                            v1 = safeGet (start, finish) distances
                            v2 = safeGet (finish, start) distances
                        in
                            (+)
                            (max v1 v2)
                            (placesToDistance distances (finish::tail))

                _ ->
                        0

findShortestDistance : Dict.Dict (String, String) Int -> Int
findShortestDistance distances =
        uniquePlaces distances
        |> Utils.getAllCombos
        |> List.map (placesToDistance distances)
        |> Utils.safeListMin

run : List String -> Int
run =
        List.foldl parseInput Dict.empty
        >> findShortestDistance
