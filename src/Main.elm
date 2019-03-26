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
    }

init : () -> ( Model, Cmd Msg )
init _ = 
    (Model 0 "" 0 0 0 "2019-3-25", Cmd.none)

type Msg
    = Input String
    | Send
    | Receive (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ({ model | item = input }, Cmd.none)
        Send ->
            ({ model | item = "posted" }, postBalance model)
        _ ->
            ({ model | item = "aaa" }, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input [ Attributes.value model.item, onInput Input ] []
            , button
                [ disabled (String.length model.item < 1) ]
                [ text "Submit" ]
            ]
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
    Decode.map6 Model
        (field "amount" Decode.int)
        (field "item" Decode.string)
        (field "kind_id" Decode.int)
        (field "purpose_id" Decode.int)
        (field "place_id" Decode.int)
        (field "date" Decode.string)
    