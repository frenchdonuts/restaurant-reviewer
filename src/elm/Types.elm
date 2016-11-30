module Types exposing (..)

import Zipper1D as Zipper
import Time.DateTime as Time


type Page
    = Home
    | RestaurantDetail String


type alias RestaurantPreview =
    { id : String
    , name : String
    , types : List String
    , address : String
    }


type alias Restaurant =
    { id : String
    , name : String
    , photos : Zipper.Zipper Img
    , address : String
    , reviews : List Review
    , avgRating : Float
    , openingHours : Maybe (List Period)
    }


type alias Img =
    { src : String
    , alt : String
    }


type alias Period =
    { open : DayTime
    , close : Maybe DayTime
    }


type alias DayTime =
    { day : Day
    , time : IntraDayTime
    }


type alias IntraDayTime =
    -- 0-23
    { hr :
        Int
        -- 0-59
    , min : Int
    }


type Day
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday


openingHoursOn : Day -> List Period -> Maybe ( DayTime, Maybe DayTime )
openingHoursOn day periods =
    List.filter (\period -> period.open.day == day) periods
        |> List.map (\period -> ( period.open, period.close ))
        |> List.head


type alias Review =
    { authorName : String
    , time : Time.DateTime
    , rating : Rating
    , text : String
    }


type alias NewReview =
    { authorName : String
    , time : Maybe Time.DateTime
    , rating : Rating
    , text : String
    }


initNewReview : NewReview
initNewReview =
    { authorName = ""
    , time = Nothing
    , rating = One
    , text = ""
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
