import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..) 
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Http exposing (..)

main : Program () Model Msg
main =
    Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

type alias Model =
    { amount : Int
    , item : String
    , kind_id : Int
    , purpose_id : Int
    , place_id : Int
    , date : String
    , tmp : String
    }

init : () -> ( Model, Cmd Msg )
init _ = 
    (Model 0 "" 0 0 0 "" "", Cmd.none)

type Msg
    = Input String
    | Send
    | Receive (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            if (String.split " " input |> List.length) == 2 then
                ( updateModel model model.tmp, Cmd.none)
            else
                ({ model | tmp = input }, Cmd.none)
        Send ->
            ({ model | item = "posted" }, postBalance model)
        _ ->
            ( model, Cmd.none)

updateModel : Model -> String -> Model
updateModel model str =
    if model.amount == 0 then
        case String.toInt str of
            Just value ->
                { model | amount = value, tmp = "" }
            Nothing ->
                { model | tmp = "" }
    else if model.item == "" then
        { model | item = str, tmp = "" }
    else if model.kind_id == 0 then
        case String.toInt str of
            Just value ->
                { model | kind_id = value, tmp = "" }
            Nothing ->
                { model | tmp = "" }
    else if model.purpose_id == 0 then
        case String.toInt str of
            Just value ->
                { model | purpose_id = value, tmp = "" }
            Nothing ->
                { model | tmp = "" }
    else if model.place_id == 0 then
        case String.toInt str of
            Just value ->
                { model | place_id = value, tmp = "" }
            Nothing ->
                { model | tmp = "" }
    else if model.date == "" then
        { model | date = str, tmp = "" }
    else
        model

view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input [ Attributes.value model.tmp, onInput Input ] []
            , button
                [ disabled (String.length model.tmp < 1) ]
                [ text "Submit" ]
            ]
        , text <| String.fromInt model.amount
        , br [] []
        , text model.item
        , br [] []
        , text <| String.fromInt model.kind_id
        , br [] []
        , text <| String.fromInt model.purpose_id
        , br [] []
        , text <| String.fromInt model.place_id
        , br [] []
        , text model.date
        ]

postBalance : Model -> Cmd Msg
postBalance model =
    let
        url =
            "http://localhost:8080/api/v1/balance/"
        body =
            encodeBalance model
                |> Http.jsonBody
    in
        Http.request
            { method = "POST"
            , headers = []
            , url = url
            , body = body
            , expect = Http.expectJson Receive Decode.string
            , timeout = Nothing
            , tracker = Nothing
            }

encodeBalance : Model -> Encode.Value
encodeBalance model =
    Encode.object
        [ ("amount", Encode.int model.amount)
        , ("item", Encode.string model.item)
        , ("kind_id", Encode.int model.kind_id)
        , ("purpose_id", Encode.int model.purpose_id)
        , ("place_id", Encode.int model.place_id)
        , ("date", Encode.string model.date)
        ]

decodeBalance : Decode.Decoder Model
decodeBalance =
    Decode.map7 Model
        (field "amount" Decode.int)
        (field "item" Decode.string)
        (field "kind_id" Decode.int)
        (field "purpose_id" Decode.int)
        (field "place_id" Decode.int)
        (field "date" Decode.string)
        (field "tmp" Decode.string)
    
type alias Balance =
    { amount : Int
    , item : String
    , kind_id : Int
    , purpose_id : Int
    , place_id : Int
    , date : String
    }


