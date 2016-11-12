module Types exposing (..)

import Date


type alias RestaurantPreview =
    { name : String
    , types : List String
    , photos : List String
    , address : String
    }


type alias Review =
    { reviewer : String
    , date : Date.Date
    , rating : Rating
    , comments : List Comment
    }


type alias Comment =
    { author : String
    , comment : String
    }


type Rating
    = One
    | Two
    | Three
    | Four
    | Five


type alias Filters =
    { cuisine : Cuisine
    , openNow : Bool
    , maxPrice : Price
    }


type Price
    = Steal
    | Deal
    | Casual
    | Fine
    | Fancy


type Cuisine
    = NoPreference
    | African
    | American
    | AsianFusion
    | Bakery
    | Breakfast
    | Brunch
    | Chinese
    | Dessert
    | Dinner
    | Indian
    | Italian
    | Japanese
    | Korean
    | Lunch
    | Mediterranean
    | Mexican
    | MiddleEastern
    | Vietnamese
