module Main exposing (..)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Update
import View
import Html.App as App
import Nav
import Navigation


-- APP


main : Program Never
main =
    Navigation.program Nav.urlParser
        { init = init
        , urlUpdate = urlUpdate
        , view = View.root
        , update = Update.update
        , subscriptions = Update.subscriptions
        }


init : Result String Page -> ( Model, Cmd Msg )
init parseResult =
    let
        ( initModel, initCmd ) =
            Update.mockRestaurantDetailInit

        ( updateModel, updateCmd ) =
            urlUpdate parseResult initModel
    in
        updateModel ! [ initCmd, updateCmd ]


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate parseResult model =
    case parseResult of
        Ok page ->
            ( { model | currentPage = RestaurantDetail "" }, Cmd.none )

        Err errMsg ->
            ( { model | currentPage = Home }, Cmd.none )
