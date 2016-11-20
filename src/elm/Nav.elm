module Nav exposing (..)

import Types exposing (..)
import Navigation as Nav exposing (Location)
import UrlParser exposing (Parser, parse, s, string, (</>), format)
import String


toPath : Page -> String
toPath page =
    case page of
        Home ->
            ""

        RestaurantDetail id ->
            "restaurant/" ++ id


urlParser : Nav.Parser (Result String Page)
urlParser =
    -- makeParser : (Location -> a) -> Parser a
    Nav.makeParser pathParser


pathParser : Location -> Result String Page
pathParser location =
    let
        { pathname } =
            location
    in
        -- parse : formatter -> Parser formatter a -> String -> Result String a
        -- (b -> b) -> Parser (b -> b) a -> String -> Result String a
        -- a needs to be Page
        -- : formatter -> Parser formatter Page -> String -> Result String Page
        -- formatter needs to be : Parser Page Page
        parse identity parser (String.dropLeft 1 pathname)


{-| s : String -> Parser a a
-- int : Parser (Int -> a) a
-- oneOf : List (Parser a b) Parser a b
-- </> : Parser a b -> Parser b c -> Parser a c  "parser composition"
-- s "" : Parser a a
-- s "restaurant" </> int : Parser a a -> Parser (Int -> a) a -> Parser a a
-}
parser : Parser (Page -> a) a
parser =
    UrlParser.oneOf
        -- format :: formatter -> Parser formatter a -> Parser (a -> result) result
        [ format Home <| s ""
          -- :: (Int -> Page) -> Parser (Int -> b) b -> Parser (Page -> a) a
        , format RestaurantDetail <| s "restaurant" </> string
        ]
