module View exposing (root)

import Model exposing (..)
import Msg exposing (..)
import Views.Pages.Home as Home exposing (view)
import Html exposing (Html, div, ul, text, img, span)
import Html.Attributes as Attr
import Material.Layout as Layout
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)


root : Model -> Html Msg
root model =
    let
        main =
            div
                [ Attr.style
                    [ ( "margin", "auto" )
                    , ( "padding-top", "2%" )
                    , ( "padding-left", "8%" )
                    , ( "padding-right", "8%" )
                      --, ( "min-height", "500px" )
                    ]
                ]
                [ Home.view model ]
    in
        Layout.render Mdl
            model.mdl
            [ Layout.waterfall True ]
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
                [ Layout.href "https://github.com/debois/elm-mdl" ]
                [ span [] [ text "github" ] ]
            ]
        ]
    ]
