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



{-
   init : Result String Page -> ( Model, Cmd Msg )
   init parseResult =
       let
           ( initModel, initCmd ) =
               Update.init

           ( updateModel, updateCmd ) =
               urlUpdate parseResult initModel
       in
           updateModel ! [ initCmd, updateCmd ]


   urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
   urlUpdate parseResult model =
       case parseResult of
           Ok page ->
               ( { model | currentPage = page }, Cmd.none )

           Err errMsg ->
               ( { model | currentPage = Home }, Cmd.none )
-}
