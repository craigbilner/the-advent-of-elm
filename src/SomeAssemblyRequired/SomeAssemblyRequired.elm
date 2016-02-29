module SomeAssemblyRequired where

import Dict
import Bitwise
import Regex
import Debug
import String


import Utils

type Rule = VALUE String Int
          | AND String String String
          | OR String String String
          | NOT String String
          | LSHIFT String String Int
          | RSHIFT String String Int
          | BINDING String String
          | NoOp

ruleRegex : String
ruleRegex = "([0-9]+|[a-z]+) -> ([a-z]+)|([a-z]+|[0-9]+) (AND|OR|LSHIFT|RSHIFT) ([a-z]+|[0-9]+) -> ([a-z]+)|(NOT) ([a-z]+) -> ([a-z]+)"

instructionToRule : String -> Rule
instructionToRule instruction =
        let
            submatches = Regex.find Regex.All (Regex.regex ruleRegex) instruction
                         |> Utils.matchesToSubmatches
        in
            case submatches of
                    key1::op::key2::key::tail ->
                            case op of
                                    "AND" ->
                                            AND key key1 key2

                                    "OR" ->
                                            OR key key1 key2

                                    "LSHIFT" ->
                                            LSHIFT key key1 (Utils.safeToInt key2)

                                    "RSHIFT" ->
                                            RSHIFT key key1 (Utils.safeToInt key2)

                                    _ ->
                                            Debug.crash "Op unknown"
                    op::key1::key::tail ->
                            case op of
                                    "NOT" ->
                                            NOT key key1

                                    _ ->
                                            Debug.crash "Not a NOT"

                    value::key::tail ->
                            case String.toInt value of
                                    Ok num ->
                                            VALUE key num

                                    _ ->
                                            BINDING key value

                    _ ->
                            Debug.crash ("Unknown rule: " ++ instruction)

evaluateAND : Dict.Dict String Int -> String -> String -> String -> Maybe (String, Int)
evaluateAND values key key1 key2 =
        let
            value1 =
                    if Utils.isInt key1
                        then Just (Utils.safeToInt key1)
                        else Dict.get key1 values

            value2 = Dict.get key2 values

            vp = (value1, value2)
        in
            case vp of
                    (Just v1, Just v2) ->
                            Just (key, Bitwise.and v1 v2)

                    _ ->
                            Nothing

evaluateOR : Dict.Dict String Int -> String -> String -> String -> Maybe (String, Int)
evaluateOR values key key1 key2 =
        let
            value1 = Dict.get key1 values

            value2 = Dict.get key2 values

            v = (value1, value2)
        in
            case v of
                    (Just v1, Just v2) ->
                            Just (key, Bitwise.or v1 v2)

                    _ ->
                            Nothing

evaluateNOT : Dict.Dict String Int -> String -> String -> Maybe (String, Int)
evaluateNOT values key key1 =
        let
            value1 = Dict.get key1 values
        in
            case value1 of
                    Just v1 ->
                            Just (key, Bitwise.and 0xffff (Bitwise.complement v1))

                    _ ->
                            Nothing

evaluateLShift : Dict.Dict String Int -> String -> String -> Int -> Maybe (String, Int)
evaluateLShift values key key1 num =
        let
            value1 = Dict.get key1 values
        in
            case value1 of
                    Just v1 ->
                            Just (key, Bitwise.shiftLeft v1 num)

                    _ ->
                            Nothing

evaluateRShift : Dict.Dict String Int -> String -> String -> Int -> Maybe (String, Int)
evaluateRShift values key key1 num =
        let
            value1 = Dict.get key1 values
        in
            case value1 of
                    Just v1 ->
                            Just (key, Bitwise.shiftRight v1 num)

                    _ ->
                            Nothing

evaluateBinding : Dict.Dict String Int -> String -> String -> Maybe (String, Int)
evaluateBinding values key key1 =
       let
            value1 = Dict.get key1 values
       in
            case value1 of
                    Just v1 ->
                            Just (key, v1)

                    _ ->
                            Nothing

evaluateRule : Dict.Dict String Int -> Rule -> Maybe (String, Int)
evaluateRule values rule =
        case rule of
                VALUE key num ->
                        Just (key, num)

                AND key key1 key2 ->
                        evaluateAND values key key1 key2

                OR key key1 key2 ->
                        evaluateOR values key key1 key2

                NOT key key1 ->
                        evaluateNOT values key key1 

                LSHIFT key key1 num ->
                        evaluateLShift values key key1 num

                RSHIFT key key1 num ->
                        evaluateRShift values key key1 num

                BINDING key key1 ->
                        evaluateBinding values key key1

                NoOp ->
                        Debug.crash "Unknown rule"

parseToModel : String -> (Dict.Dict String Int, List Rule) -> (Dict.Dict String Int, List Rule)
parseToModel instruction (values, rules) =
        let
            rule = instructionToRule instruction
            result = evaluateRule values rule
        in
            case result of
                    Just (key, value) ->
                            (Dict.insert key value values, rules)

                    _ ->
                            (values, rule::rules)

runAllRules : Dict.Dict String Int -> List Rule -> Dict.Dict String Int
runAllRules values rules =
        case rules of
                [] ->
                        values

                h::t ->
                        let
                            result = evaluateRule values h
                        in
                           case result of
                                   Just (key, value) ->
                                           runAllRules (Dict.insert key value values) t

                                   _ ->
                                           runAllRules values (List.append t [h])

run : List String -> String -> Maybe Int
run instructions wire =
        let
            parsedModel = List.foldl parseToModel (Dict.empty, []) instructions
            allValues = runAllRules (fst parsedModel) (snd parsedModel)
        in
            Dict.get wire allValues
