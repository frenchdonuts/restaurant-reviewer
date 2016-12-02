module Nav exposing (..)

import Types exposing (Page(..))
import UrlParser as Url exposing (Parser, top, s, string, (</>), map)


toPath : Page -> String
toPath page =
    case page of
        Home ->
            ""

        RestaurantDetail id ->
            "restaurant/" ++ id


route : Url.Parser (Page -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map RestaurantDetail (s "restaurant" </> string)
        ]
