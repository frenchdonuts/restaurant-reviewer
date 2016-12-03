module Utils exposing (..)

import Http


{-| "?k1=v1&k2=v2&...kn=vn"
-}
url : String -> List ( String, String ) -> String
url base parameters =
    let
        encodeParameters parameters =
            case parameters of
                ( k, v ) :: [] ->
                    Http.encodeUri k ++ "=" ++ (Http.encodeUri v)

                ( k, v ) :: kvs ->
                    Http.encodeUri k ++ "=" ++ (Http.encodeUri v) ++ "&" ++ (encodeParameters kvs)

                [] ->
                    ""
    in
        base ++ "?" ++ (encodeParameters parameters)


maybeToBool : Maybe Bool -> Bool
maybeToBool mb =
    case mb of
        Just b ->
            b

        Nothing ->
            False


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        _ ->
            False


isNothing : Maybe a -> Bool
isNothing =
    not << isJust


oneOf : List (Maybe a) -> Maybe a
oneOf ms =
    case ms of
        (Just a) :: ms ->
            Just a

        Nothing :: ms ->
            oneOf ms

        [] ->
            Nothing
