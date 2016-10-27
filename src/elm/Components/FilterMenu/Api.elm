module Components.FilterMenu.Api exposing (queryParameters)

import Components.FilterMenu.Types exposing (..)


queryParameters : State -> List ( String, String )
queryParameters (State { openNow, cuisine, maxPrice }) =
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

        African ->
            "african"

        AsianFusion ->
            "asian fusion"

        American ->
            "american"

        Bakery ->
            "bakery"

        Breakfast ->
            "breakfast"

        Brunch ->
            "brunch"

        Chinese ->
            "chinese"

        Dessert ->
            "dessert"

        Dinner ->
            "dinner"

        Indian ->
            "indian"

        Italian ->
            "italian"

        Japanese ->
            "japanese"

        Korean ->
            "korean"

        Lunch ->
            "lunch"

        Mediterranean ->
            "mediterranean"

        Mexican ->
            "mexican"

        MiddleEastern ->
            "middle eastern"

        Vietnamese ->
            "vietnamese"
