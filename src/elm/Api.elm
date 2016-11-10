module Api exposing (getRestaurants)

import Model exposing (..)
import Types exposing (..)
import Helper
import Components.Autocomplete exposing (getSelectedDatum)
import Task
import Http
import Json.Decode as Json exposing ((:=))


getRestaurants : Float -> Float -> Model -> Task.Task Http.Error (List Restaurant)
getRestaurants lat long model =
    let
        api_key =
            "AIzaSyBFF9RccdIGE7dOBQdiq8m0EPGNJH51pmg"

        latlong =
            toString lat ++ "," ++ toString long

        parameters =
            queryParameters model
                ++ [ ( "key", api_key )
                   , ( "location", latlong )
                   , ( "radius", "500" )
                   ]

        url =
            Http.url
                "https://maps.googleapis.com/maps/api/place/textsearch/json"
                parameters
    in
        Http.get ("results" := Json.list decodeRestaurant) url


decodeRestaurant : Json.Decoder Restaurant
decodeRestaurant =
    Json.object4 Restaurant
        ("name" := Json.string)
        ("types" := Json.list Json.string)
        ("icon" := Json.string)
        ("formatted_address" := Json.string)


queryParameters : Model -> List ( String, String )
queryParameters { includeCasualInSearch, includeFancyInSearch, openNow, cuisineAutocomplete } =
    let
        selectedCuisine =
            case getSelectedDatum cuisineAutocomplete of
                Just cuisine ->
                    cuisine

                Nothing ->
                    NoPreference

        minprice =
            case includeCasualInSearch of
                True ->
                    "0"

                False ->
                    "3"

        maxprice =
            case includeFancyInSearch of
                True ->
                    "4"

                False ->
                    "2"

        parameters =
            [ ( "query", cuisineString selectedCuisine ++ " restaurant" )
            , ( "minprice", minprice )
            , ( "maxprice", maxprice )
            ]
    in
        if openNow then
            ( "opennow", "" ) :: parameters
        else
            parameters


cuisineString : Cuisine -> String
cuisineString cuisine =
    case cuisine of
        NoPreference ->
            ""

        _ ->
            Helper.cuisineString cuisine
