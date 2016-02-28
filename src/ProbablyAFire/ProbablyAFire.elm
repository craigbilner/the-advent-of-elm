module ProbablyAFire where

import String
import Regex


import Utils

type Action
        = On (Int, Int) (Int, Int)
        | Off (Int, Int) (Int, Int)
        | Toggle (Int, Int) (Int, Int)
        | NoOp

type alias Range = { x1 : Int
                   , y1 : Int
                   , x2 : Int
                   , y2 : Int
                   }

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

getLeftBit : Range -> Range -> List Range
getLeftBit block toSplit =
        if toSplit.x1 < block.x1
           then [Range toSplit.x1 toSplit.y1 (block.x1 - 1) toSplit.y2]
           else []

getRightBit : Range -> Range -> List Range
getRightBit block toSplit =
        if toSplit.x2 > block.x2
           then [Range (block.x2 + 1) toSplit.y1 toSplit.x2 toSplit.y2]
           else []

getTopBit : Range -> Range -> List Range
getTopBit block toSplit =
        if toSplit.y2 > block.y2
           then [Range (max toSplit.x1 block.x1) (block.y2 + 1) (min toSplit.x2 block.x2) toSplit.y2]
           else []

getBottomBit : Range -> Range -> List Range
getBottomBit block toSplit =
        if toSplit.y1 < block.y1
           then [Range (max toSplit.x1 block.x1) toSplit.y1 (min toSplit.x2 block.x2) (block.y1 - 1)]
           else []

splitRangeAroundBlock : Range -> Range -> List Range
splitRangeAroundBlock block toSplit =
        let
            leftBit = getLeftBit block toSplit
            rightBit = getRightBit block toSplit
            topBit = getTopBit block toSplit
            bottomBit = getBottomBit block toSplit
        in
            Utils.flattenNonEmpties [leftBit, rightBit, topBit, bottomBit]

determineSplit : Range -> Range -> List Range
determineSplit block toSplit =
        if toSplit.x2 < block.x1 then
            --to the left
            [toSplit]

        else if toSplit.x1 > block.x2 then
            --to the right
            [toSplit]

        else if toSplit.y1 > block.y2 then
            --to the top
            [toSplit]

        else if toSplit.y2 < block.y1 then
            --to the bottom
            [toSplit]

        else if toSplit.x1 >= block.x1
             && toSplit.x2 <= block.x2
             && toSplit.y1 >= block.y1
             && toSplit.y2 <= block.y2 then
            --is inside
            []

        else
            splitRangeAroundBlock block toSplit

concatSplits : Range -> Range -> List Range -> List Range
concatSplits block toSplit splits =
        List.append splits (determineSplit block toSplit)

splitPerBlock : Range -> List Range -> List Range
splitPerBlock block splits =
        List.foldl (concatSplits block) [] splits

splitRange : Range -> List Range -> List Range
splitRange toSplit blocks =
        List.foldl splitPerBlock [toSplit] blocks

update : Action -> List Range -> List Range
update action turnedOn =
        case action of
                On start end ->
                        let
                            range = Range (fst start) (snd start) (fst end) (snd end)
                        in
                            if List.isEmpty turnedOn
                               then [range]
                               else List.append (splitRange range turnedOn) turnedOn

                Off start end ->
                        let
                            range = Range (fst start) (snd start) (fst end) (snd end)
                        in
                            if List.isEmpty turnedOn
                               then []
                               else splitPerBlock range turnedOn

                Toggle start end ->
                        let
                            range = Range (fst start) (snd start) (fst end) (snd end)
                        in
                            if List.isEmpty turnedOn
                               then
                                   [range]
                               else
                                   let
                                       lightsToGoOn = splitRange range turnedOn
                                       afterLightsOff = splitPerBlock range turnedOn
                                   in
                                       List.append lightsToGoOn afterLightsOff

                NoOp ->
                        turnedOn

changeLights : String -> List Range -> List Range
changeLights instruction turnedOn =
        turnedOn
        |> update (instructionToAction instruction)

rangesToCount : Range -> Int -> Int
rangesToCount {x2, x1, y2, y1} count =
            count + ((x2 - x1 + 1) * (y2 - y1 + 1))

run : List String -> Int
run instructions =
        instructions
        |> List.foldl changeLights []
        |> List.foldl rangesToCount 0
