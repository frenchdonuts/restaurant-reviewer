module Configs exposing (..)

import Types exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Components.Autocomplete as Autocomplete
import Helper exposing (cuisineString)


-- Cuisine Autocomplete


cuisineAutocompleteUpdateConfig : Autocomplete.UpdateConfig Msg Cuisine
cuisineAutocompleteUpdateConfig =
    { toId = cuisineString
    , onSelectChoice = SelectedCuisine
    }


cuisineAutocompleteViewConfig : Autocomplete.ViewConfig Cuisine
cuisineAutocompleteViewConfig =
    let
        customLi keySelected mouseSelected i cuisine =
            { attributes =
                [ id <| Autocomplete.descendantId cuisineString cuisine
                , classList
                    [ ( "mdl-list__item", True )
                    , ( "active", keySelected || mouseSelected )
                    ]
                , style [ ( "background-color", "white" ) ]
                , attribute "role" "option"
                ]
            , children = [ text (cuisineString cuisine) ]
            }
    in
        { toId = cuisineString
        , ul =
            [ class "mdl-list"
            , style
                [ ( "position", "absolute" )
                , ( "width", "100%" )
                , ( "z-index", "2" )
                , ( "margin-top", "-19px" )
                ]
            ]
        , li = customLi
        , inputLabel = "Select cuisine type"
        }
