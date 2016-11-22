module Helper exposing (..)

import Types exposing (..)


prices : List Price
prices =
    [ Steal
    , Deal
    , Casual
    , Fine
    , Fancy
    ]


priceString : Price -> String
priceString price =
    case price of
        Steal ->
            "Steal"

        Deal ->
            "Deal"

        Casual ->
            "Casual"

        Fine ->
            "Fine"

        Fancy ->
            "Fancy"


cuisines : List Cuisine
cuisines =
    [ NoPreference
    , African
    , American
    , AsianFusion
    , Bakery
    , Breakfast
    , Brunch
    , Chinese
    , Dessert
    , Dinner
    , Indian
    , Italian
    , Japanese
    , Korean
    , Lunch
    , Mediterranean
    , Mexican
    , MiddleEastern
    , Vietnamese
    ]


cuisineString : Cuisine -> String
cuisineString cuisine =
    case cuisine of
        NoPreference ->
            "No Preference"

        African ->
            "African"

        AsianFusion ->
            "Asian Fusion"

        American ->
            "American"

        Bakery ->
            "Bakery"

        Breakfast ->
            "Breakfast"

        Brunch ->
            "Brunch"

        Chinese ->
            "Chinese"

        Dessert ->
            "Dessert"

        Dinner ->
            "Dinner"

        Indian ->
            "Indian"

        Italian ->
            "Italian"

        Japanese ->
            "Japanese"

        Korean ->
            "Korean"

        Lunch ->
            "Lunch"

        Mediterranean ->
            "Mediterranean"

        Mexican ->
            "Mexican"

        MiddleEastern ->
            "Middle Eastern"

        Vietnamese ->
            "Vietnamese"


cuisineStringInverse : String -> Cuisine
cuisineStringInverse id =
    case id of
        "No Preference" ->
            NoPreference

        "African" ->
            African

        "Asian Fusion" ->
            AsianFusion

        "American" ->
            American

        "Bakery" ->
            Bakery

        "Breakfast" ->
            Breakfast

        "Brunch" ->
            Brunch

        "Chinese" ->
            Chinese

        "Dessert" ->
            Dessert

        "Dinner" ->
            Dinner

        "Indian" ->
            Indian

        "Italian" ->
            Italian

        "Japanese" ->
            Japanese

        "Korean" ->
            Korean

        "Lunch" ->
            Lunch

        "Mediterranean" ->
            Mediterranean

        "Mexican" ->
            Mexican

        "Middle Eastern" ->
            MiddleEastern

        "Vietnamese" ->
            Vietnamese

        _ ->
            NoPreference


intToRating : Int -> Maybe Rating
intToRating i =
    case i of
        1 ->
            Just One

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        _ ->
            Nothing


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
