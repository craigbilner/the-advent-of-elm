module Src.DoesntHeHaveInterns.DoesntHeHaveInterns where

import String
import Dict


import Src.Utils as Utils

vowels : List String
vowels = ["a", "e", "i", "o", "u"]

hasThreeVowels : String -> Maybe (Int, String) -> Bool -> Maybe (Int, String)
hasThreeVowels letter result hasCompleted =
        case result of
                Just (count, _) ->
                        let
                            newCount = if List.member letter vowels
                                           then count + 1
                                           else count
                        in
                            if hasCompleted
                                then if newCount >= 3
                                         then Just (newCount, "")
                                         else Nothing
                                else Just (newCount, "")

                Nothing ->
                        Nothing

hasDoubleLetter : String -> Maybe (Int, String) -> Bool -> Maybe (Int, String)
hasDoubleLetter letter result hasCompleted =
        case result of
                Just (count, lastLetter) ->
                        let
                            newCount = if lastLetter == letter
                                           then count + 1
                                           else count
                        in
                            if hasCompleted
                                then if newCount > 0
                                         then Just (newCount, letter)
                                         else Nothing
                                else Just (newCount, letter)
                Nothing ->
                        Nothing

bannedPairs : Dict.Dict String String
bannedPairs =
        Dict.fromList [ ("a", "b")
                      , ("c", "d")
                      , ("p", "q")
                      , ("x", "y")
                      ]

hasBannedSequence : String -> Maybe (Int, String) -> Bool -> Maybe (Int, String)
hasBannedSequence letter result hasCompleted =
        case result of
                Just (_, lastLetter) ->
                        case Dict.get lastLetter bannedPairs of
                                Just bannedLetter ->
                                        if letter == bannedLetter
                                            then Nothing
                                            else Just (0, letter)

                                Nothing ->
                                        Just (0, letter)

                Nothing ->
                        Nothing

performTransformation : String -> Bool -> (String -> a -> Bool -> a) -> a -> a
performTransformation letter hasCompleted rule result =
        rule letter result hasCompleted

isValid : List (Maybe (Int, String)) -> List String -> Bool
isValid results letters =
        case letters of
                [] ->
                        not (List.any Utils.isNothing results)
                (x::xs) ->
                        let
                            rules = [ hasThreeVowels
                                    , hasDoubleLetter
                                    , hasBannedSequence
                                    ]
                            hasCompleted = List.isEmpty xs
                            transforms = List.map2 (performTransformation x hasCompleted) rules results         
                            hasFailure = List.any Utils.isNothing transforms
                        in
                            if hasFailure
                               then isValid [Nothing] []
                               else isValid transforms xs

validPasswordToBit : String -> Int -> Int
validPasswordToBit password count =
        let
            passwordIsValid =
                    isValid [Just (0, ""), Just (0, ""), Just(0, "")] (String.split "" password)
        in
            if passwordIsValid
               then count + 1
               else count

run : List String -> Int
run passwords =
      List.foldl validPasswordToBit 0 passwords
