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
                (
                    { model
                        | balance = Balance.interpretation input
                        , tmp = input
                    } 
                    , Cmd.none
                )
            Send ->
                ({ model | balance = Balance.init, inputStatus = None, tmp = "" }, Balance.encode model.balance |> balancePost)
            _ ->
                ( model, Cmd.none)
    else if model.inputStatus == Move then
        case msg of
            Input input ->
                (
                    { model
                        | attributeMove = AttributeMove.interpretation input
                        , tmp = input
                    } 
                    , Cmd.none
                )
            Send ->
                ({ model | attributeMove = AttributeMove.init, inputStatus = None, tmp = "" }, AttributeMove.encode model.attributeMove |> attributeMovePost)
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
        , Balance.htmlMsg model.balance
        , br [] []
        , AttributeMove.htmlMsg model.attributeMove
        ]

transStatusToString : InputStatus -> String
transStatusToString status =
    case status of 
        Out -> "out"
        Move -> "move"
        None -> "none"

balancePost : Encode.Value -> Cmd Msg
balancePost = getDomain ++ "/api/v1/balance/" |> post

attributeMovePost : Encode.Value -> Cmd Msg
attributeMovePost = getDomain ++ "/api/v1/move/" |> post

post : String -> Encode.Value -> Cmd Msg
post url encode =
    let
        body = encode |> Http.jsonBody
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

