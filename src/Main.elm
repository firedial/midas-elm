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
    , attributeMove : AttributeMove
    , tmp : String
    , inputStatus : InputStatus
    }

init : () -> ( Model, Cmd Msg )
init _ = 
    (Model (Balance 0 "" 0 0 0 "") (AttributeMove "" 0 0 0 "") "" None, Cmd.none)

type Msg
    = Input String
    | Send
    | Receive (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.inputStatus == Out then
        case msg of
            Input input ->
                if (String.split " " input |> List.length) == 2 then
                    ( updateBalanceModel model model.tmp, Cmd.none)
                else
                    ({ model | tmp = input }, Cmd.none)
            Send ->
                ({ model | balance = Balance 0 "" 0 0 0 "", inputStatus = None }, postBalance model.balance)
            _ ->
                ( model, Cmd.none)
    else if model.inputStatus == Move then
        case msg of
            Input input ->
                if (String.split " " input |> List.length) == 2 then
                    ( updateAttributeMoveModel model model.tmp, Cmd.none)
                else
                    ({ model | tmp = input }, Cmd.none)
            Send ->
                ({ model | attributeMove = AttributeMove "" 0 0 0 "", inputStatus = None }, postAttributeMove model.attributeMove)
            _ ->
                ( model, Cmd.none)
    else
        case msg of
            Input input ->
                if (String.split " " input |> List.length) == 2 then
                    if model.tmp == "out" then
                        ({ model | inputStatus = Out, tmp = ""}, Cmd.none)
                    else if model.tmp == "move" then
                        ({ model | inputStatus = Move, tmp = ""}, Cmd.none)
                    else
                        ({ model | tmp = "" }, Cmd.none)
                else
                    ({ model | tmp = input }, Cmd.none)
            _ ->
                ( model, Cmd.none)

updateAttributeMoveModel : Model -> String -> Model
updateAttributeMoveModel model str =
    { model | attributeMove = updateAttributeMove model.attributeMove str, tmp = "" }

updateAttributeMove : AttributeMove -> String -> AttributeMove
updateAttributeMove attributeMove str =
    if attributeMove.attribute == "" then
        { attributeMove | attribute = str }
    else if attributeMove.amount == 0 then
        case String.toInt str of
            Just value ->
                { attributeMove | amount = value }
            Nothing ->
                attributeMove 
    else if attributeMove.beforeId == 0 then
        case String.toInt str of
            Just value ->
                { attributeMove | beforeId = value }
            Nothing ->
                attributeMove 
    else if attributeMove.afterId == 0 then
        case String.toInt str of
            Just value ->
                { attributeMove | afterId = value }
            Nothing ->
                attributeMove 
    else if attributeMove.date == "" then
        { attributeMove | date = str }
    else
        attributeMove



updateBalanceModel : Model -> String -> Model
updateBalanceModel model str =
    { model | balance = updateBalance model.balance str, tmp = "" }

updateBalance : Balance -> String -> Balance
updateBalance balance str =
    if balance.amount == 0 then
        case String.toInt str of
            Just value ->
                { balance | amount = -1 * value }
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
        , br [] []
        , text <| transStatusToString model.inputStatus
        , br [] []
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
        , br [] []
        , br [] []
        , text model.attributeMove.attribute
        , br [] []
        , text <| String.fromInt model.attributeMove.amount
        , br [] []
        , text <| String.fromInt model.attributeMove.beforeId
        , br [] []
        , text <| String.fromInt model.attributeMove.afterId
        , br [] []
        , text model.attributeMove.date
        , br [] []
        ]

transStatusToString : InputStatus -> String
transStatusToString status =
    case status of 
        Out -> "out"
        Move -> "move"
        None -> "none"

postAttributeMove : AttributeMove -> Cmd Msg
postAttributeMove attributeMove =
    let
        url =
            "http://192.168.1.6:8080/api/v1/move/"
        body =
            encodeAttributeMove attributeMove
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

encodeAttributeMove : AttributeMove -> Encode.Value
encodeAttributeMove attributeMove =
    Encode.object
        [ ("attribute", Encode.string attributeMove.attribute)
        , ("amount", Encode.int attributeMove.amount)
        , ("before_id", Encode.int attributeMove.beforeId)
        , ("after_id", Encode.int attributeMove.afterId)
        , ("date", Encode.string attributeMove.date)
        ]

postBalance : Balance -> Cmd Msg
postBalance balance =
    let
        url =
            "http://192.168.1.6:8080/api/v1/balance/"
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

type InputStatus = Out | Move | None

type alias AttributeMove =
    { attribute : String
    , amount : Int
    , beforeId : Int
    , afterId : Int
    , date : String
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

        



