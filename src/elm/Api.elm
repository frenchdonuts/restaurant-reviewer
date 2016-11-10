module Api exposing (getRestaurants)

import Types exposing (..)
import Helper
import Task
import Http
import Json.Decode as Json exposing ((:=))


getRestaurants : Float -> Float -> Filters -> Task.Task Http.Error (List Restaurant)
getRestaurants lat long filterMenu =
    let
        api_key =
            "AIzaSyBFF9RccdIGE7dOBQdiq8m0EPGNJH51pmg"

        latlong =
            toString lat ++ "," ++ toString long

        parameters =
            queryParameters filterMenu
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


queryParameters : Filters -> List ( String, String )
queryParameters { openNow, cuisine, maxPrice } =
    let
        maxprice =
            case maxPrice of
                Steal ->
                    "0"

                Deal ->
                    "1"

                Casual ->
                    "2"

                Fine ->
                    "3"

                Fancy ->
                    "4"

        parameters =
            [ ( "query", cuisineString cuisine ++ " restaurant" )
            , ( "minprice", "0" )
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
