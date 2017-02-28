module Main exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Update
import View
import Navigation


-- APP


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = Update.init
        , view = View.root
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
