module Main exposing (..)

import Update
import View
import Html.App as App


-- APP


main : Program Never
main =
    App.program
        { init = Update.init
        , view = View.root
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
