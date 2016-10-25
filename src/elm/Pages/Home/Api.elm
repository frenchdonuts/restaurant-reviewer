module Pages.Home.Api exposing (..)

import Pages.Home.Types exposing (..)
import Components.FilterMenu.Types as FilterMenu
import Components.FilterMenu.Api as FilterMenu
import Task
import Http
import Json.Decode as Json exposing ((:=))


getRestaurants : Float -> Float -> FilterMenu.State -> Task.Task Http.Error (List Restaurant)
getRestaurants lat long filterMenu =
    let
        api_key =
            "AIzaSyBFF9RccdIGE7dOBQdiq8m0EPGNJH51pmg"

        latlong =
            toString lat ++ "," ++ toString long

        parameters =
            FilterMenu.queryParameters filterMenu
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
