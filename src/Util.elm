module Util exposing (..)

import List

find : (a -> Bool) -> List a -> Maybe a
find isGood searchedList =
    List.filter isGood searchedList
        |> List.head

isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just n -> True
        Nothing -> False

memberCallback : (a -> Bool) -> List a -> Bool
memberCallback isGood searchedList =
    find isGood searchedList
        |> isJust

dropTail : Int -> List a -> List a
dropTail elementCount list =
    List.reverse list
        |> List.drop elementCount
        |> List.reverse

