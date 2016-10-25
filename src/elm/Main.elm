module Main exposing (..)

import Html.App
import Components.FilterMenu.View as View
import Components.FilterMenu.State as State


--import Pages.Home.View as View
--import Pages.Home.State as State
-- APP


main : Program Never
main =
    Html.App.program
        { init = ( State.init, Cmd.none )
        , view = View.root
        , update = (\b a -> ( State.update b a, Cmd.none ))
        , subscriptions = \_ -> Sub.none
        }
