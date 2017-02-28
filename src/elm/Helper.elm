module Helper exposing (..)

{-| This module mainly defines functions on our Types defined in the Types module
-}

import Types exposing (..)


(+|+) : Bool -> List a -> List a -> List a
(+|+) append xs2 xs1 =
    if append then
        xs1 ++ xs2
    else
        xs1


(||>) : (a -> b -> c) -> (a -> a) -> (a -> b -> c)
(||>) f t =
    (\a b -> f (t a) b)


(>>=) : ( model, Cmd msg ) -> (model -> ( model, Cmd msg )) -> ( model, Cmd msg )
(>>=) ( model, cmd ) update =
    let
        ( newModel, moreCmds ) =
            update model
    in
        newModel ! [ cmd, moreCmds ]


prices : List Price
prices =
    [ Casual
    , Fancy
    ]


priceString : Price -> String
priceString price =
    case price of
        Casual ->
            "Casual"

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


cuisineToMaybe : Cuisine -> Maybe Cuisine
cuisineToMaybe cuisine =
    case cuisine of
        NoPreference ->
            Nothing

        _ ->
            Just cuisine


{-| maybeToCuisine Nothing == maybeToCuisine (Just NoPreference)
-}
maybeToCuisine : Maybe Cuisine -> Cuisine
maybeToCuisine mCuisine =
    case mCuisine of
        Nothing ->
            NoPreference

        Just cuisine ->
            cuisine


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


incRating : Rating -> Rating
incRating =
    ratingToInt >> (+) 1 >> intToRating


decRating : Rating -> Rating
decRating =
    ratingToInt >> (-) 1 >> intToRating


lt : Rating -> Rating -> Bool
lt r1 r2 =
    let
        z1 =
            ratingToInt r1

        z2 =
            ratingToInt r2
    in
        z1 < z2


lte : Rating -> Rating -> Bool
lte r1 r2 =
    (lt r1 r2) || (r1 == r2)


intToRating : Int -> Rating
intToRating z =
    case z of
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
            if z < 1 then
                One
            else
                Five


ratingToInt : Rating -> Int
ratingToInt rating =
    case rating of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5
