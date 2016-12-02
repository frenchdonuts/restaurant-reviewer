module View exposing (root)

import Model exposing (..)
import Msg exposing (..)
import Types exposing (..)
import Views.Pages.Home as Home
import Views.Pages.RestaurantDetail as RestaurantDetail
import Html exposing (Html, div, ul, text, img, span)
import Material.Layout as Layout
import Material.Options as Options exposing (css)


root : Model -> Html Msg
root model =
    let
        currentView =
            case model.currentPage of
                Home ->
                    Home.view model

                RestaurantDetail _ ->
                    RestaurantDetail.view model

        main =
            div
                []
                [ currentView ]
    in
        Layout.render Mdl
            model.mdl
            [ Layout.scrolling ]
            { header = header model
            , drawer = []
            , tabs = ( [], [] )
            , main = [ main ]
            }


header : Model -> List (Html Msg)
header model =
    [ Layout.row
        [ css "transition" "height 333ms ease-in-out 0s" ]
        [ Layout.title [] [ text "restaurant-reviewer" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/frenchdonuts/restaurant-reviewer" ]
                [ span [] [ text "github" ] ]
            ]
        ]
    ]
