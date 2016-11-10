module View exposing (root)

import Model exposing (..)
import Msg exposing (..)
import Views.Pages.Home as Home exposing (view)
import Html exposing (Html, div, ul, text, img, span)
import Material.Layout as Layout
import Material.Options as Options exposing (css)


root : Model -> Html Msg
root model =
    Layout.render Mdl
        model.mdl
        [ Layout.waterfall True ]
        { header = header model
        , drawer = []
        , tabs = ( [], [] )
        , main = [ Home.view model ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row
        [ css "transition" "height 333ms ease-in-out 0s" ]
        [ Layout.title [] [ text "restaurant-reviewer" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/debois/elm-mdl" ]
                [ span [] [ text "github" ] ]
            ]
        ]
    ]
