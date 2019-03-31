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
    { balance : Balance
    , tmp : String
    }

init : () -> ( Model, Cmd Msg )
init _ = 
    (Model (Balance 0 "" 0 0 0 "") "", Cmd.none)

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
            ({ model | balance = Balance 0 "" 0 0 0 "" }, postBalance model.balance)
        _ ->
            ( model, Cmd.none)

updateModel : Model -> String -> Model
updateModel model str =
    { model | balance = updateBalance model.balance str, tmp = "" }

updateBalance : Balance -> String -> Balance
updateBalance balance str =
    if balance.amount == 0 then
        case String.toInt str of
            Just value ->
                { balance | amount = value }
            Nothing ->
                balance 
    else if balance.item == "" then
        { balance | item = str }
    else if balance.kind_id == 0 then
        case String.toInt str of
            Just value ->
                { balance | kind_id = value }
            Nothing ->
                balance 
    else if balance.purpose_id == 0 then
        case String.toInt str of
            Just value ->
                { balance | purpose_id = value }
            Nothing ->
                balance 
    else if balance.place_id == 0 then
        case String.toInt str of
            Just value ->
                { balance | place_id = value }
            Nothing ->
                balance 
    else if balance.date == "" then
        { balance | date = str }
    else
        balance

view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input [ Attributes.value model.tmp, onInput Input ] []
            , button
                [ disabled (String.length model.tmp < 1) ]
                [ text "Submit" ]
            ]
        , text <| String.fromInt model.balance.amount
        , br [] []
        , text model.balance.item
        , br [] []
        , text <| String.fromInt model.balance.kind_id
        , br [] []
        , text <| String.fromInt model.balance.purpose_id
        , br [] []
        , text <| String.fromInt model.balance.place_id
        , br [] []
        , text model.balance.date
        ]

postBalance : Balance -> Cmd Msg
postBalance balance =
    let
        url =
            "http://localhost:8080/api/v1/balance/"
        body =
            encodeBalance balance
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

encodeBalance : Balance -> Encode.Value
encodeBalance balance =
    Encode.object
        [ ("amount", Encode.int balance.amount)
        , ("item", Encode.string balance.item)
        , ("kind_id", Encode.int balance.kind_id)
        , ("purpose_id", Encode.int balance.purpose_id)
        , ("place_id", Encode.int balance.place_id)
        , ("date", Encode.string balance.date)
        ]

decodeBalance : Decode.Decoder Balance
decodeBalance =
    Decode.map6 Balance
        (field "amount" Decode.int)
        (field "item" Decode.string)
        (field "kind_id" Decode.int)
        (field "purpose_id" Decode.int)
        (field "place_id" Decode.int)
        (field "date" Decode.string)
    
type alias Balance =
    { amount : Int
    , item : String
    , kind_id : Int
    , purpose_id : Int
    , place_id : Int
    , date : String
    }

type alias Attribute =
    { id : Int
    , name : String
    , description : String
    , group_id : Int
    }

enableAttribute : List Attribute -> String -> List Attribute
enableAttribute attributes str =
    List.filter (isStartStringAttribute str) attributes

isStartStringAttribute : String -> Attribute -> Bool
isStartStringAttribute splitString attribute =
    isStartString splitString attribute.name

isStartString : String -> String -> Bool
isStartString splitString separatedString =
    let
        str = String.split splitString separatedString |> List.head
    in
        case str of
            Just s ->
                if s == "" then
                    True
                else
                    False
            Nothing ->
                False

        



