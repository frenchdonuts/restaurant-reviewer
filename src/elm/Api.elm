module Api exposing (getRestaurants, getRestaurant, mockGetRestaurant)

import Model exposing (..)
import Types exposing (..)
import Helper
import Utils
import Zipper1D as Zipper
import Components.Autocomplete exposing (getSelectedDatum)
import Task
import Http
import Json.Decode as Json exposing (field)
import String
import ParseInt
import Time.DateTime as Time


api_key : String
api_key =
    "AIzaSyBFF9RccdIGE7dOBQdiq8m0EPGNJH51pmg"


getRestaurants : Float -> Float -> Model -> Http.Request (List RestaurantPreview)
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
            Utils.url
                "https://maps.googleapis.com/maps/api/place/textsearch/json"
                parameters
                |> Debug.log "url for restaurants fetch"

        mockUrl =
            "http://localhost:8080/static/restaurants_response.json"
    in
        Http.get url (field "results" (Json.list decodeRestaurantPreview))


mockGetRestaurant : String -> Http.Request Restaurant
mockGetRestaurant id =
    let
        mockUrl =
            "http://localhost:8080/static/restaurantDetail_response.json"
    in
        Http.get mockUrl (field "result" decodeRestaurant)


getRestaurant : String -> Http.Request Restaurant
getRestaurant id =
    let
        parameters =
            [ ( "key", api_key )
            , ( "placeid", id )
            ]

        url =
            Utils.url
                "https://maps.googleapis.com/maps/api/place/details/json"
                parameters
    in
        Http.get url (field "result" decodeRestaurant)


decodeRestaurant : Json.Decoder Restaurant
decodeRestaurant =
    let
        createZipperOfPhotos photos =
            let
                noPhotosAvailableAssetUrl =
                    ""

                imgs =
                    List.map (\src -> Img src "Restaurant image. No description available") photos
            in
                case imgs of
                    [] ->
                        Zipper.zipper [] (Img noPhotosAvailableAssetUrl "No photos available for this restaurant.") []

                    x :: xs ->
                        Zipper.zipper [] x xs

        decodeListOfPhotosAsZipper =
            Json.map createZipperOfPhotos (field "photos" (Json.list <| Json.map toPhotoUrl (field "photo_reference" Json.string)))
    in
        Json.map7 Restaurant
            (field "id" Json.string)
            (field "name" Json.string)
            decodeListOfPhotosAsZipper
            (field "formatted_address" Json.string)
            (field "reviews" <| Json.list decodeReview)
            (field "rating" Json.float)
            (Json.maybe << field "opening_hours" << field "periods" <| Json.list decodePeriod)


decodePeriod : Json.Decoder Period
decodePeriod =
    Json.map2 Period
        (field "open" decodeDayTime)
        (field "close" <| Json.maybe decodeDayTime)


decodeDayTime : Json.Decoder DayTime
decodeDayTime =
    Json.map2 DayTime
        (field "day" <| Json.map toDay Json.int)
        (field "time" decodeTime)


decodeTime : Json.Decoder IntraDayTime
decodeTime =
    let
        parseTime string =
            let
                hrString =
                    String.left 2 string

                minString =
                    String.dropLeft 2 string

                hr =
                    case (ParseInt.parseInt hrString) of
                        Ok hr ->
                            hr

                        Err err ->
                            Debug.log "Err parsing hr: " err
                                |> (\_ -> -1)

                min =
                    case (ParseInt.parseInt minString) of
                        Ok min ->
                            min

                        Err err ->
                            Debug.log "Err parsing min: " err
                                |> (\_ -> -1)
            in
                IntraDayTime hr min
    in
        Json.map parseTime Json.string


decodeReview : Json.Decoder Review
decodeReview =
    Json.map4 Review
        (field "author_name" Json.string)
        (field "time" <| Json.map (Time.fromTimestamp << ((*) 1000)) Json.float)
        (field "rating" <| Json.map toRating Json.int)
        (field "text" Json.string)


toDay : Int -> Day
toDay i =
    case i of
        0 ->
            Sunday

        1 ->
            Monday

        2 ->
            Tuesday

        3 ->
            Wednesday

        4 ->
            Thursday

        5 ->
            Friday

        6 ->
            Saturday

        _ ->
            Monday


toRating : Int -> Rating
toRating i =
    case i of
        1 ->
            One

        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        _ ->
            One


{-|
    This will translate the photo_reference our initial search returned to the
    actual API query needed to grab the restaurant image.
-}
toPhotoUrl : String -> String
toPhotoUrl reference =
    let
        api_key2 =
            "AIzaSyAifY5S5j1r_UMB5Ze3zAFI9YQmKnaak_Q"
    in
        "https://maps.googleapis.com/maps/api/place/photo?maxwidth=533&photoreference="
            ++ reference
            ++ "&key="
            ++ api_key


decodeRestaurantPreview : Json.Decoder RestaurantPreview
decodeRestaurantPreview =
    Json.map4 RestaurantPreview
        (field "place_id" Json.string)
        (field "name" Json.string)
        (field "types" <| Json.list Json.string)
        (field "formatted_address" Json.string)


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
