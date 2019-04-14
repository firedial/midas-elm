import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..) 
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Http exposing (..)

import Model.Balance as Balance exposing (..)
import Model.AttributeMove as AttributeMove exposing (..)
import Config.Env exposing (..)
import Model.Model as Model exposing (..)

main : Program () Model Msg
main =
    Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

init : () -> ( Model, Cmd Msg )
init _ = 
    (Model Balance.init AttributeMove.init "" None, Cmd.none)

type Msg
    = Input String
    | Send
    | Receive (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.inputStatus == Out then
        case msg of
            Input input ->
                ( updateBalanceModel model input, Cmd.none)
            Send ->
                ({ model | balance = Balance.init, inputStatus = None }, postBalance model.balance)
            _ ->
                ( model, Cmd.none)
    else if model.inputStatus == Move then
        case msg of
            Input input ->
                ( updateAttributeMoveModel model input, Cmd.none)
            Send ->
                ({ model | attributeMove = AttributeMove.init, inputStatus = None }, postAttributeMove model.attributeMove)
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
    { model | attributeMove = AttributeMove.interpretation str, tmp = str }

updateBalanceModel : Model -> String -> Model
updateBalanceModel model str =
    { model | balance = Balance.interpretation str, tmp = str }

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
        , text <| maybeIntText model.balance.amount
        , br [] []
        , text <| maybeStringText model.balance.item
        , br [] []
        , text <| maybeIntText model.balance.kind_id
        , br [] []
        , text <| maybeIntText model.balance.purpose_id
        , br [] []
        , text <| maybeIntText model.balance.place_id
        , br [] []
        , text <| maybeStringText model.balance.date
        , br [] []
        , br [] []
        , text <| maybeStringText model.attributeMove.attribute
        , br [] []
        , text <| maybeIntText model.attributeMove.amount
        , br [] []
        , text <| maybeIntText model.attributeMove.beforeId
        , br [] []
        , text <| maybeIntText model.attributeMove.afterId
        , br [] []
        , text <| maybeStringText model.attributeMove.date
        , br [] []
        ]

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
            getDomain ++ "/api/v1/move/"
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
    Encode.object [ ("item", Encode.string "aaa") ]
        -- [ ("attribute", Encode.string attributeMove.attribute)
        -- , ("amount", Encode.int attributeMove.amount)
        -- , ("before_id", Encode.int attributeMove.beforeId)
        -- , ("after_id", Encode.int attributeMove.afterId)
        -- , ("date", Encode.string attributeMove.date)
        -- ]

postBalance : Balance -> Cmd Msg
postBalance balance =
    let
        url =
            getDomain ++ "/api/v1/balance/"
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
    Encode.object [ ("item", Encode.string "aaa") ]
--         [ ("amount", Encode.int balance.amount)
--         , ("item", Encode.string balance.item)
--         , ("kind_id", Encode.int balance.kind_id)
--         , ("purpose_id", Encode.int balance.purpose_id)
--         , ("place_id", Encode.int balance.place_id)
--         , ("date", Encode.string balance.date)
--         ]
-- 
-- decodeBalance : Decode.Decoder Balance
-- decodeBalance =
--     Decode.map6 Balance
--         (field "amount" Decode.int)
--         (field "item" Decode.string)
--         (field "kind_id" Decode.int)
--         (field "purpose_id" Decode.int)
--         (field "place_id" Decode.int)
--         (field "date" Decode.string)
    


