module Model.Balance exposing (Balance, init, interpretation, encode)

import Json.Encode as Encode exposing (..)

type alias Balance =
    { amount : Maybe Int
    , item : Maybe String
    , kind_id : Maybe Int
    , purpose_id : Maybe Int
    , place_id : Maybe Int
    , date : Maybe String
    }

interpretation : String -> Balance
interpretation str =
    let
        stringList = String.split " " str |> Maybe.Just

        maybeHead = Maybe.andThen List.head
        maybeTail = Maybe.andThen List.tail
        maybeInt = Maybe.andThen String.toInt

        amount = stringList |> maybeHead |> maybeInt
        item = stringList |> maybeTail |> maybeHead
        kind_id = stringList |> maybeTail |> maybeTail |> maybeHead |> maybeInt
        purpose_id = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> maybeInt
        place_id = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> maybeInt
        date = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead
    in
        Balance amount item kind_id purpose_id place_id date
 
stringizing : Balance -> List String
stringizing balance =
    let
        amount = maybeIntText balance.amount 
        item = maybeStringText balance.item
        kind_id = maybeIntText balance.kind_id
        purpose_id = maybeIntText balance.purpose_id
        place_id = maybeIntText balance.place_id
        date = maybeStringText balance.date
    in
        amount :: item :: kind_id :: purpose_id :: place_id :: date :: []

init : Balance
init =
    Balance Nothing Nothing Nothing Nothing Nothing Nothing

encode : Balance -> Encode.Value
encode balance =
    Encode.object
        [ ("amount", Encode.int <| maybeIntNumber balance.amount)
        , ("item", Encode.string <| maybeStringText balance.item)
        , ("kind_id", Encode.int <| maybeIntNumber balance.kind_id)
        , ("purpose_id", Encode.int <| maybeIntNumber balance.purpose_id)
        , ("place_id", Encode.int <| maybeIntNumber balance.place_id)
        , ("date", Encode.string <| maybeStringText balance.date)
        ]

-- decodeBalance : Decode.Decoder Balance
-- decodeBalance =
--     Decode.map6 Balance
--         (field "amount" Decode.int)
--         (field "item" Decode.string)
--         (field "kind_id" Decode.int)
--         (field "purpose_id" Decode.int)
--         (field "place_id" Decode.int)
--         (field "date" Decode.string)
    




maybeIntNumber : Maybe Int -> Int
maybeIntNumber n =
    case n of
        Nothing ->
            0
        Just k ->
            k


maybeIntText : Maybe Int -> String
maybeIntText n =
    case n of
        Nothing ->
            ""
        Just k ->
            String.fromInt k

maybeStringText : Maybe String -> String
maybeStringText n =
    case n of
        Nothing ->
            ""
        Just k ->
            k

