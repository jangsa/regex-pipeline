module Helper exposing (..)

import Maybe
import Regex
import Json.Decode as JD

applyRegex : String -> String -> List Regex.Match
applyRegex regex text =
    if String.length regex > 0 then
        Regex.find (by regex) text

    else
        []


by : String -> Regex.Regex
by regex =
    Maybe.withDefault Regex.never <| Regex.fromString regex


toInt : String -> Int
toInt mi =
    case String.toInt mi of
        Just i ->
            i

        Nothing ->
            0


getAt : Int -> List a -> Maybe a
getAt index xs =
    if index > List.length xs then
        Nothing

    else if index < 0 then
        Nothing

    else
        List.drop index xs |> List.head

-- WARNING: It's NOT tail-recursive!
replace : Int -> a -> List a -> List a
replace target y xs =
    case xs of
        [] ->
            []

        h :: t ->
            if target < 0 then
                xs

            else if target > List.length xs then
                xs

            else if target == 0 then
                y :: t

            else
                h :: replace (target - 1) y t


