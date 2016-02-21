module Src.PerfectlySpherical.PerfectlySpherical where

import String
import Set


import Src.Utils as Utils

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
directionsToSet direction uniquePositions =
        case uniquePositions of
                (x::xs) ->
                    let
                        newPosition = Utils.addTuples (directionToCoordinate direction) x
                    in
                        newPosition::uniquePositions

                _ ->
                    uniquePositions

run : String -> Int
run directions =
        String.split "" directions
        |> List.foldl directionsToSet [(0,0)]
        |> Set.fromList
        |> Set.size
        
