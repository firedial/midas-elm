module Balance exposing (Balance, interpretation, init)

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
 
init : Balance
init =
    Balance Nothing Nothing Nothing Nothing Nothing Nothing

