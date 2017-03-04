module Api exposing (getRestaurants, getRestaurant, mockGetRestaurant)

import Model exposing (..)
import Types exposing (..)
import Helper
import Utils
import Zipper1D as Zipper
import Task
import Http
import Json.Decode as Json exposing (field)
import String
import ParseInt
import Time.DateTime as Time


proxy_url : String
proxy_url = "https://localhost:3000"

getRestaurants : Float -> Float -> Cuisine -> Model -> Http.Request (List RestaurantPreview)
getRestaurants lat long selectedCuisine model =
    let
        latlong =
            toString lat ++ "," ++ toString long

        parameters =
            queryParameters selectedCuisine model
                ++ [ ( "location", latlong )
                   , ( "radius", "500" )
                   ]

        url =
            Utils.url
                (proxy_url ++ "/maps/api/place/textsearch/json")
                parameters

        mockUrl =
            "http://localhost:8080/static/restaurants_response.json"
    in
        Http.get url (field "results" (Json.list decodeRestaurantPreview))


queryParameters : Cuisine -> Model -> List ( String, String )
queryParameters cuisine { priceFilter, includeOnlyOpenRestaurants, cuisineAutocomplete } =
    let
        ( minprice, maxprice ) =
            case priceFilter of
                IncludeBoth ->
                    ( "0", "4" )

                IncludeJustFancy ->
                    ( "3", "4" )

                IncludeJustCasual ->
                    ( "0", "2" )

        parameters =
            [ ( "query", cuisineString cuisine ++ " restaurant" )
              --, ( "minprice", minprice )
              --, ( "maxprice", maxprice )
            ]
    in
        -- TODO: Use includeOnlyOpenRestaurants
        parameters



--if openNow then
--    ( "opennow", "" ) :: parameters
--else
--    parameters


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
            [ ( "placeid", id )
            ]

        url =
            Utils.url
                (proxy_url ++ "/maps/api/place/details/json")
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
    "https://maps.googleapis.com/maps/api/place/photo?maxwidth=533&photoreference="
        ++ reference


decodeRestaurantPreview : Json.Decoder RestaurantPreview
decodeRestaurantPreview =
    Json.map6 RestaurantPreview
        (field "place_id" Json.string)
        (field "name" Json.string)
        (field "types" <| Json.list Json.string)
        (field "formatted_address" Json.string)
        (Json.maybe << field "opening_hours" <| field "open_now" Json.bool)
        (Json.maybe <| field "price_level" Json.int)


cuisineString : Cuisine -> String
cuisineString cuisine =
    case cuisine of
        NoPreference ->
            ""

        _ ->
            Helper.cuisineString cuisine
