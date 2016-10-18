module Pages.Home.Api exposing (..)

import Pages.Home.Types exposing (..)
import Task
import Http
import Json.Decode as Json exposing ((:=))


getRestaurants : Float -> Float -> Task.Task Http.Error (List Restaurant)
getRestaurants lat long =
    let
        api_key =
            "AIzaSyBFF9RccdIGE7dOBQdiq8m0EPGNJH51pmg"

        latlong =
            toString lat ++ "," ++ toString long

        url =
            Http.url
                "https://maps.googleapis.com/maps/api/place/textsearch/json"
                [ ( "query", "restaurant" )
                , ( "key", api_key )
                , ( "location", latlong )
                , ( "radius", "500" )
                ]
    in
        Http.get ("results" := Json.list decodeRestaurant) url


decodeRestaurant : Json.Decoder Restaurant
decodeRestaurant =
    Json.object4 Restaurant
        ("name" := Json.string)
        ("types" := Json.list Json.string)
        ("icon" := Json.string)
        ("formatted_address" := Json.string)
