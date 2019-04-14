module Model.AttributeMove exposing (AttributeMove, init, interpretation, encode)

import Json.Encode as Encode exposing (..)

type alias AttributeMove =
    { attribute : Maybe String
    , amount : Maybe Int
    , beforeId : Maybe Int
    , afterId : Maybe Int
    , date : Maybe String
    }

interpretation : String -> AttributeMove
interpretation str =
    let
        stringList = String.split " " str |> Maybe.Just

        maybeHead = Maybe.andThen List.head
        maybeTail = Maybe.andThen List.tail
        maybeInt = Maybe.andThen String.toInt

        attribute = stringList |> maybeHead 
        amount = stringList |> maybeTail |> maybeHead |> maybeInt
        beforeId = stringList |> maybeTail |> maybeTail |> maybeHead |> maybeInt
        afterId = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> maybeInt
        date = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead
    in
        AttributeMove attribute amount beforeId afterId date
 
init : AttributeMove
init =
    AttributeMove Nothing Nothing Nothing Nothing Nothing

encode : AttributeMove -> Encode.Value
encode attributeMove =
    Encode.object
        [ ("attribute", Encode.string <| maybeStringText attributeMove.attribute)
        , ("amount", Encode.int <| maybeIntNumber attributeMove.amount)
        , ("before_id", Encode.int <| maybeIntNumber attributeMove.beforeId)
        , ("after_id", Encode.int <| maybeIntNumber attributeMove.afterId)
        , ("date", Encode.string <| maybeStringText attributeMove.date)
        ]




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


