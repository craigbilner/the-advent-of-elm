module Src.IWasToldThereWouldBeNoMath.IWasToldThereWouldBeNoMath where

import String
import Src.Utils exposing (safeToInt, safeListMin, double)

type alias Dimensions =
        { l : Int
        , w : Int
        , h : Int
        }

dimensionsFromList : List Int -> Dimensions
dimensionsFromList dimensionList =
        case dimensionList of
                [l, w, h] ->
                        Dimensions l w h
                _ ->
                        Dimensions 0 0 0

dimensionsFromString : String -> Dimensions
dimensionsFromString dimensions =
        String.split "x" dimensions
        |> List.map safeToInt
        |> dimensionsFromList

calculateSurfaceArea : Dimensions -> Int
calculateSurfaceArea {l, w, h} =
        let
            side1 =
                l * w
            side2 =
                w * h
            side3 =
                h * l
            sa =
                [side1, side2, side3]
                |> List.map double
                |> List.sum
            extra =
                safeListMin [side1, side2, side3]
        in
            sa + extra

toTotalSurfaceArea :  String -> Int -> Int
toTotalSurfaceArea dimensionsString total =
        let
            sa =
                dimensionsString
                |> dimensionsFromString
                |> calculateSurfaceArea
        in
            total + sa
            
run : List String -> Int
run presentDimensions =
        List.foldl toTotalSurfaceArea 0 presentDimensions
