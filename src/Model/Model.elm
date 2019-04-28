module Model.Model exposing (Model, InputStatus(..))

import Model.Balance as Balance exposing (..)
import Model.AttributeMove as AttributeMove exposing (..)
import Model.Attribute as Attribute exposing (..)

type InputStatus = Out | Move | None

type alias Model =
    { balance : Balance
    , attributeMove : AttributeMove
    , tmp : String
    , inputStatus : InputStatus
    , isSetKinds : Bool
    , isSetPurposes : Bool
    , isSetPlaces : Bool
    , kinds : List Attribute
    , purposes : List Attribute
    , places : List Attribute
    }
