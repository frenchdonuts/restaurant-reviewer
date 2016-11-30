module Views.Helpers exposing (..)

import Types exposing (..)
import Html exposing (Attribute)
import Html.Events exposing (on, keyCode)
import Json.Decode as Json


toStringf : Float -> String
toStringf f =
    let
        s =
            toString f
    in
        if toFloat (ceiling f) == f then
            s ++ ".0"
        else
            s


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


periodToString : Period -> String
periodToString { open, close } =
    case close of
        Just { time } ->
            (dayTimeToString open) ++ "-" ++ (timeToString time)

        Nothing ->
            "Open 24/7"


dayTimeToString : DayTime -> String
dayTimeToString { day, time } =
    dayToString day ++ ", " ++ timeToString time


timeToString24 : IntraDayTime -> String
timeToString24 { hr, min } =
    (toString hr) ++ ":" ++ (toString min)


timeToString : IntraDayTime -> String
timeToString { hr, min } =
    let
        ampm =
            if hr > 11 then
                "pm"
            else
                "am"

        mod12 =
            hr % 12
    in
        (toString mod12) ++ ":" ++ (make2Digit min) ++ ampm


make2Digit : Int -> String
make2Digit i =
    if i < 10 then
        "0" ++ (toString i)
    else
        toString i


dayToString : Day -> String
dayToString day =
    case day of
        Sunday ->
            "Sunday"

        Monday ->
            "Monday"

        Tuesday ->
            "Tuesday"

        Wednesday ->
            "Wednesday"

        Thursday ->
            "Thursday"

        Friday ->
            "Friday"

        Saturday ->
            "Saturday"


ratingToString : Rating -> String
ratingToString rating =
    case rating of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"
