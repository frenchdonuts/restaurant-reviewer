module Api exposing (getRestaurants)

import Model exposing (..)
import Types exposing (..)
import Helper
import Components.Autocomplete exposing (getSelectedDatum)
import Task
import Http
import Json.Decode as Json exposing ((:=))


api_key : String
api_key =
    "AIzaSyBFF9RccdIGE7dOBQdiq8m0EPGNJH51pmg"


getRestaurants : Float -> Float -> Model -> Task.Task Http.Error (List RestaurantPreview)
getRestaurants lat long model =
    let
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

        mockUrl =
            "http://localhost:8080/static/restaurants_response.json"

        photoReferenceToUrl restaurants =
            List.map (\restaurant -> { restaurant | photos = List.map toPhotoUrl restaurant.photos }) restaurants
    in
        Http.get ("results" := Json.list decodeRestaurant) mockUrl
            |> Task.map photoReferenceToUrl
            |> Debug.log "hit11111111"


{-|
    This will translate the photo_reference our initial search returned to the
    actual API query needed to grab the restaurant image.
-}
toPhotoUrl : String -> String
toPhotoUrl reference =
    "https://maps.googleapis.com/maps/api/place/photo?maxwidth=400&photoreference="
        ++ reference
        ++ "key="
        ++ api_key


decodeRestaurant : Json.Decoder RestaurantPreview
decodeRestaurant =
    Json.object4 RestaurantPreview
        ("name" := Json.string)
        ("types" := Json.list Json.string)
        ("photos" := Json.list ("photo_reference" := Json.string))
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
