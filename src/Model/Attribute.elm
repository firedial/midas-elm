module Model.Attribute exposing (Attribute, decodeAttributes, resultAttributes)

import Json.Decode as Decode exposing (..)

type alias Attribute =
    { id : Int
    , name : String
    , description : String
    , group_id : Int
    }

resultAttributes : Result Error (List Attribute) -> List Attribute
resultAttributes result =
    case result of
        Ok attributes ->
            attributes
        _ ->
            []

-- decodeAttributes : String -> Result Error (List Attribute)
-- decodeAttributes str =
--     decodeString (list decodeAttribute) str

decodeAttributes : Decode.Decoder (List Attribute)
decodeAttributes =
    list decodeAttribute

decodeAttribute : Decode.Decoder Attribute
decodeAttribute =
    Decode.map4 Attribute
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "description" Decode.string)
        (field "group_id" Decode.int)



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


 