module Msg exposing (..)

import Types exposing (..)
import Geolocation
import Material
import Components.Autocomplete as Autocomplete
import Time.DateTime as Time
import Time as CoreTime


type Msg
    = NoOp
      -- Initialization
    | OnInitErr String
    | OnInitSuc Geolocation.Location
      -- Restaurants API Request
    | FetchRestaurants
    | OnFetchRestaurantsErr String
    | OnFetchRestaurantsSuc (List RestaurantPreview)
      -- Restaurant API Response
    | OnFetchRestaurantErr String
    | OnFetchRestaurantSuc Restaurant
      -- Cuisine Selector (Autocomplete, Home page)
    | CuisineAutocomplete Autocomplete.Msg
      -- Price Selector (Home page)
    | ToggleCasual
    | ToggleFancy
      -- Open now toggle (Home page)
    | ToggleOpenNow
      -- Restaurant List (Home page)
    | OnRestaurantClick RestaurantPreview
    | MouseEnterRestaurantCard (Maybe Int)
      -- Restaurant Detail page
    | PrevPhoto
    | NextPhoto
    | OnUpdateNewReview NewReviewMsg
    | OnNewReviewSubmitBtnPressed
    | ValidNewReviewSubmitted NewReview CoreTime.Time
    | OnTimezoneOffsetFetched Int
    | Mdl (Material.Msg Msg)


type NewReviewMsg
    = UpdateName String
    | UpdateTime Time.DateTime
    | UpdateRating Rating
    | UpdateText String
