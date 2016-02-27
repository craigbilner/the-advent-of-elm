module ProbablyAFire where

import String
import Regex


import Utils

type Action
        = On (Int, Int) (Int, Int)
        | Off (Int, Int) (Int, Int)
        | Toggle (Int, Int) (Int, Int)
        | NoOp

instructionToMatches : String -> List Regex.Match
instructionToMatches =
        Regex.find Regex.All (Regex.regex "turn on|turn off|toggle|[0-9]+,[0-9]+")

stringToAction : String -> (Int, Int) -> (Int, Int) -> Action
stringToAction name start end =
        case name of
                "turn on" ->
                        On start end

                "turn off" ->
                        Off start end

                "toggle" ->
                        Toggle start end

                _ ->
                        NoOp

stringToTuple : String -> (Int, Int)
stringToTuple tuple =
        case String.split "," tuple of
                (f::s::rest) ->
                        (Utils.safeToInt f, Utils.safeToInt s)
                _ ->
                        (0, 0)

matchToAction : Maybe Regex.Match -> Maybe (List Regex.Match) -> Action
matchToAction action coords  =
        case action of
                Just rName ->
                        case coords of
                                Just (rStart::rEnd::rest) ->
                                        stringToTuple rEnd.match
                                        |> stringToAction rName.match (stringToTuple rStart.match)
                                        
                                Nothing ->
                                        NoOp

                                _ ->
                                        NoOp

                Nothing ->
                        NoOp

instructionToAction : String -> Action
instructionToAction instruction =
        let
            matches =
                    instructionToMatches instruction
        in
            matchToAction (List.head matches) (List.tail matches)

concatColumns : Int -> (Int, Int) -> List (Int, Int) -> List (Int, Int)
concatColumns y coord coords =
        let
            ys =
                    [((snd coord) + 1)..y]
            column =
                    List.map2 (,) (List.repeat (List.length ys) (fst coord)) ys
        in
            List.append coords (coord::column)

startEndToList : (Int, Int) -> (Int, Int) -> List (Int, Int)
startEndToList start end =
        let
            xs = [(fst start)..(fst end)]
        in
            snd start
            |> List.repeat (List.length xs)
            |> List.map2 (,) xs
            |> List.foldl (concatColumns (snd end)) []

partitionByIntersection : List (Int, Int) -> (Int, Int) -> Bool
partitionByIntersection listA coord =
        List.member coord listA

turnOn : List (Int, Int) -> (List (Int, Int), List (Int, Int))  -> List (Int, Int)
turnOn turnedOn (_, newLights) =
        List.append turnedOn newLights

toggle : List (Int, Int) -> List (Int, Int) -> List (Int, Int)
toggle instruction turnedOn =
        let
            (turnOff, newLights) =
                    instruction
                    |> List.partition (partitionByIntersection turnedOn)
            lightsLeftOn =
                    turnedOn
                    |> List.partition (partitionByIntersection turnOff)
                    |> snd
        in
            List.append lightsLeftOn newLights

update : Action -> List (Int, Int) -> List (Int, Int)
update action turnedOn =
        case action of
                On start end ->
                        let
                            instruction = startEndToList start end
                        in
                            if List.isEmpty turnedOn
                               then instruction
                               else instruction
                                    |> List.partition (partitionByIntersection turnedOn)
                                    |> turnOn turnedOn

                Off start end ->
                        let
                            instruction = startEndToList start end
                        in
                            if List.isEmpty turnedOn
                               then turnedOn
                               else turnedOn
                                    |> List.partition (partitionByIntersection instruction)
                                    |> snd

                Toggle start end ->                        
                        let
                            instruction = startEndToList start end
                        in
                            if List.isEmpty turnedOn
                               then instruction
                               else toggle instruction turnedOn

                NoOp ->
                        turnedOn

changeLights : String -> List (Int, Int) -> List (Int, Int)
changeLights instruction turnedOn =
        turnedOn
        |> update (instructionToAction instruction)

run : List String -> Int
run instructions =
        List.foldl changeLights [] instructions
        |> List.length
