module Model.Attribute exposing (Attribute)

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

 