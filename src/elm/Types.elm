module Types exposing (..)


type alias Restaurant =
    { name : String
    , types : List String
    , photoUrl : String
    , address : String
    }


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
