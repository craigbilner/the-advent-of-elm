module SomeAssemblyRequired where

import Dict
import Bitwise
import Regex
import Debug
import String

import Utils

type alias Model = { values : Dict.Dict String Int
                   , rules : List Rule 
                   }

type Rule = VALUE String Int
          | AND String String String
          | OR String String String
          | NOT String String
          | LSHIFT String String Int
          | RSHIFT String String Int
          | BINDING String String
          | NoOp

initModel : Model
initModel = Model Dict.empty []

ruleRegex : String
ruleRegex = "([0-9]+|[a-z]+) -> ([a-z]+)|([a-z]+) (AND|OR|LSHIFT|RSHIFT) ([a-z]+|[0-9]+) -> ([a-z]+)|(NOT) ([a-z]+) -> ([a-z]+)"

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
                                            NoOp
                    op::key1::key::tail ->
                            case op of
                                    "NOT" ->
                                            NOT key key1

                                    _ ->
                                            NoOp

                    value::key::tail ->
                            case String.toInt value of
                                    Ok num ->
                                            VALUE key num

                                    _ ->
                                            BINDING key value

                    _ ->
                            Debug.crash ("Unknown rule: " ++ instruction)

evaluateAND : Model -> String -> String -> String -> Model
evaluateAND model key key1 key2 =
        let
            value1 = Dict.get key1 model.values
            value2 = Dict.get key2 model.values
            v = (value1, value2)
        in
            case v of
                    (Just v1, Just v2) ->
                            let
                                rule = AND key key1 key2
                            in
                                { model | values = (Dict.insert key (Bitwise.and v1 v2) model.values)
                                        , rules = (removeRule rule model.rules)
                                }
                    _ ->
                            { model | rules = ((AND key key1 key2)::model.rules) }

evaluateOR : Model -> String -> String -> String -> Model
evaluateOR model key key1 key2 =
        let
            value1 = Dict.get key1 model.values
            value2 = Dict.get key2 model.values
            v = (value1, value2)
        in
            case v of
                    (Just v1, Just v2) ->
                            let
                                rule = OR key key1 key2
                            in
                                { model | values = (Dict.insert key (Bitwise.or v1 v2) model.values)
                                        , rules = (removeRule rule model.rules)
                                }
                    _ ->
                            { model | rules = ((OR key key1 key2)::model.rules) }

evaluateNOT : Model -> String -> String -> Model
evaluateNOT model key key1 =
        let
            value1 = Dict.get key1 model.values
        in
            case value1 of
                    Just v1 ->
                            let
                                rule = NOT key key1
                                result = Bitwise.and 0xffff (Bitwise.complement v1) 
                            in
                                { model | values = (Dict.insert key result model.values)
                                        , rules = (removeRule rule model.rules)
                                }
                    _ ->
                            { model | rules = ((NOT key key1)::model.rules) }

evaluateLShift : Model -> String -> String -> Int -> Model
evaluateLShift model key key1 num =
        let
            value1 = Dict.get key1 model.values
        in
            case value1 of
                    Just v1 ->
                            let
                                rule = LSHIFT key key1 num
                            in
                                { model | values = (Dict.insert key (Bitwise.shiftLeft v1 num) model.values)
                                        , rules = (removeRule rule model.rules)
                                }
                    _ ->
                            { model | rules = ((LSHIFT key key1 num)::model.rules) }

evaluateRShift : Model -> String -> String -> Int -> Model
evaluateRShift model key key1 num =
        let
            value1 = Dict.get key1 model.values
        in
            case value1 of
                    Just v1 ->
                            let
                                rule = RSHIFT key key1 num
                            in
                                { model | values = (Dict.insert key (Bitwise.shiftRight v1 num) model.values) 
                                        , rules = (removeRule rule model.rules)
                                }
                    _ ->
                            { model | rules = ((RSHIFT key key1 num)::model.rules) }

evaluateBinding : Model -> String -> String -> Model
evaluateBinding model key key1 =
       let
            value1 = Dict.get key1 model.values
       in
            case value1 of
                    Just v1 ->
                            let
                                rule = BINDING key key1
                            in
                                { model | values = (Dict.insert key v1 model.values) 
                                        , rules = (removeRule rule model.rules)
                                }
                    _ ->
                            { model | rules = ((BINDING key key1)::model.rules) }

removeRule : Rule -> List Rule -> List Rule
removeRule rule =
        List.filter (\a -> a /= rule)

evaluateRule : Model -> Rule -> Model
evaluateRule model rule =
        case rule of
                VALUE key num ->
                        { model | values = (Dict.insert key num model.values) }

                AND key key1 key2 ->
                        evaluateAND model key key1 key2

                OR key key1 key2 ->
                        evaluateOR model key key1 key2

                NOT key key1 ->
                        evaluateNOT model key key1 

                LSHIFT key key1 num ->
                        evaluateLShift model key key1 num

                RSHIFT key key1 num ->
                        evaluateRShift model key key1 num

                BINDING key key1 ->
                        evaluateBinding model key key1

                NoOp ->
                        model

parseToModel : String -> Model -> Model
parseToModel instruction model =
            instructionToRule instruction
            |> evaluateRule model

runAllRules : String -> Model -> Model
runAllRules wire model =
        let
            value = Dict.get wire model.values
        in
           case value of
                   Just val ->
                           model

                   _ ->
                           case model.rules of
                                   [] ->
                                           model

                                   h::t ->
                                           evaluateRule model h
                                           |> runAllRules wire

run : List String -> String -> Maybe Int
run instructions wire =
        let
            parsedModel = List.foldl parseToModel initModel instructions
            model = runAllRules wire parsedModel
        in
            Dict.get wire model.values
