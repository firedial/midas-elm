module View.View exposing (getView)

import Html exposing (..)

getView : Model -> Html Msg
getView model =
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
        ]
