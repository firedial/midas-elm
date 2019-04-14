module Model.AttributeMove exposing (AttributeMove, init, interpretation)

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


