module PerfectlySpherical2 where

import String
import Set


import Utils

directionToCoordinate : String -> (Int, Int)
directionToCoordinate direction =
        case direction of
                "^" ->
                        (0, 1)
                ">" ->
                        (1, 0)
                "v" ->
                        (0, -1)
                "<" ->
                        (-1, 0)
                _ ->
                        (0, 0)

directionsToSet : String -> List (Int, Int) -> List (Int, Int)
directionsToSet direction positionHistory =
        case positionHistory of
                (x::y::xs) ->
                    let
                        newPosition = Utils.addTuples (directionToCoordinate direction) y
                    in
                        newPosition::positionHistory

                _ ->
                    positionHistory

run : String -> Int
run directions =
        String.split "" directions
        |> List.foldl directionsToSet [(0,0), (0,0)]
        |> Set.fromList
        |> Set.size
