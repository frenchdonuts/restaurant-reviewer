module Msg exposing (..)

import Types exposing (..)
import Components.Autocomplete as Autocomplete
import Http
import Geolocation
import Material
import Time.DateTime as Time
import Time as CoreTime
import Navigation


type Msg
    = NoOp
      -- Initialization
    | Initialized (Result String Geolocation.Location)
      -- Restaurants API Request
    | FetchRestaurants
    | FetchedRestaurants (Result Http.Error (List RestaurantPreview))
      -- Restaurant API Response
    | FetchedRestaurant (Result Http.Error Restaurant)
      {--Home page --}
      -- Cuisine Selector (Autocomplete, Home page)
    | CuisineAutocomplete Autocomplete.Msg
    | SelectedCuisine (Maybe Cuisine)
    | OnSearchBtnPressed
    | AlertAccessibilityUser Bool
      -- Filter Menu
    | ToggleMenu
    | MouseEnterMenuItem (Maybe Int)
      -- Price Selector
    | ToggleCasual
    | ToggleFancy
      -- Open now toggle
    | ToggleOpenNow
      -- Restaurant List
    | OnRestaurantClick RestaurantPreview
    | MouseEnterRestaurantCard (Maybe Int)
      {--Restaurant Detail page --}
      -- Image Carousel
    | PrevPhoto
    | NextPhoto
      -- New Review form
    | OnUpdateNewReview NewReviewMsg
    | OnNewReviewSubmitBtnPressed
    | ValidNewReviewSubmitted NewReview CoreTime.Time
      {--Routing --}
    | UrlChange Navigation.Location
      {--Misc --}
    | OnTimezoneOffsetFetched Int
    | Mdl (Material.Msg Msg)


type NewReviewMsg
    = UpdateName String
    | UpdateTime Time.DateTime
    | UpdateRating Rating
    | UpdateText String
