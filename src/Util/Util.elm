module Util.Util exposing (getTail, getHead, getInt)

getTail : Maybe (List a) -> Maybe (List a)
getTail list =
    case list of
        Nothing ->
            Nothing
        Just lt ->
            List.tail lt

getHead : Maybe (List a) -> Maybe a
getHead list =
    case list of 
        Nothing ->
            Nothing
        Just lt ->
            List.head lt        

getInt : Maybe String -> Maybe Int
getInt str =
    case str of
        Nothing ->
            Nothing
        Just s ->
            String.toInt s

-- isDateList : List String -> Bool
-- isDateList str =
--     let
--         year = List.head str |> getInt
--         month = List.tail str |> getHead |> getInt
--         day = List.tail str |> getTail |> getHead |> getInt
--     
--     in
-- 
-- isYear : Maybe Int -> Bool
-- isYear year =
--     case year of
--         Nothing ->
--             False
--         Just y ->
--             if 1800 < y && y < 2200 then
--                 True
--             else
--                 False
-- isMonth : 

