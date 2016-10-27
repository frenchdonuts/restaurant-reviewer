module View exposing (root)

import Model exposing (..)
import Msg exposing (..)
import Views.Pages.Home as Home exposing (view)
import Html exposing (Html, div, ul, text, img)


root : Model -> Html Msg
root model =
    Home.view model
