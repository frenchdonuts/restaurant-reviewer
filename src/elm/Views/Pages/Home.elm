module Views.Pages.Home exposing (view)

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Components.Autocomplete as Autocomplete
import Configs
import Helper exposing (prices, priceString, cuisines, cuisineString, (+|+), (||>))
import Html exposing (Html, map, div, text, img, h4, select, option, p)
import Html.Attributes as Attr
import Html.Events as Events
import Material.Grid exposing (grid, cell, size, Device(..), noSpacing, Cell, offset)
import Material.List exposing (ul, li)
import Material.Card as Card
import Material.Menu as Menu
import Material.Toggles as Toggles exposing (switch, value)
import Material.Button as Button exposing (render, colored, primary)
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Icon as Icon
import Material.Typography as Typography
import Json.Decode as Json


view : Model -> Html Msg
view model =
    let
        { selectedCuisine, errMsg, cuisineAutocomplete } =
            model

        cuisineInDropdown =
            case selectedCuisine of
                Just cuisine ->
                    [ cuisine ]

                Nothing ->
                    cuisines
    in
        (Options.styled_ div)
            []
            [ Attr.style
                [ ( "min-height", "500px" ), ( "padding-top", "2%" ) ]
            ]
            [ grid
                []
                [ cell
                    [ size Desktop 4
                    , size Tablet 4
                    , size Phone 4
                    , offset Desktop 4
                    , offset Tablet 2
                    ]
                    [ Autocomplete.view
                        Configs.cuisineAutocompleteViewConfig
                        errMsg
                        cuisineAutocomplete
                        cuisines
                        |> map CuisineAutocomplete
                    ]
                , cell
                    [ size Desktop 4
                    , offset Desktop 4
                    , size Tablet 4
                    , offset Tablet 2
                    , size Phone 4
                    ]
                    [ searchButton model 3 ]
                , cell
                    [ size Desktop 10
                    , offset Desktop 1
                    , size Tablet 6
                    , offset Tablet 1
                    , size Phone 4
                    ]
                    [ listOfRestaurants model ]
                ]
              -- Offscreen p to alert accessibility Users of validation errors
            , p
                [ Attr.style
                    [ ( "position", "absolute" )
                    , ( "left", "99999999999px" )
                    ]
                , Attr.attribute "aria-live" "assertive"
                , Attr.attribute "aria-atomic" "true"
                ]
                [ text errMsg ]
            ]


searchButton : Model -> Int -> Html Msg
searchButton model idNumber =
    let
        { mdl } =
            model
    in
        Button.render Mdl
            [ idNumber ]
            mdl
            [ Button.onClick OnSearchBtnPressed
            , Button.raised
            , Button.colored
            , css "width" "100%"
            ]
            [ text "Search" ]


listOfRestaurants : Model -> Html Msg
listOfRestaurants model =
    let
        { restaurants } =
            model

        restaurantGrid =
            if List.isEmpty restaurants then
                grid
                    [ css "background-color" "transparent"
                    , css "width" "100%"
                    , noSpacing
                    ]
                    [ placeHolder
                    ]
            else
                grid
                    [ css "background-color" "transparent"
                    , css "width" "100%"
                    , noSpacing
                    , Options.attribute <| Attr.attribute "role" "list"
                    ]
                    (restaurants
                        |> filterRestaurants model
                        |> List.indexedMap cardCell
                    )

        placeHolder =
            cell
                [ size Desktop 12
                , size Tablet 8
                , size Phone 4
                , css "height" <| (toString placeHolderHeight) ++ "px"
                , Color.background <| Color.color Color.Grey Color.S200
                , css "text-align" "center"
                , css "padding-top" <| (toString <| placeHolderHeight / 2 - 17) ++ "px"
                , Typography.display1
                ]
                [ Html.text "No restaurants to display" ]

        placeHolderHeight =
            400

        cardCell i restaurant =
            cell
                [ size Desktop 6
                , size Tablet 8
                , size Phone 4
                ]
                [ restaurantCard model i restaurant ]
    in
        Options.div
            []
            [ Options.div
                [ css "height" "32px"
                , Color.background Color.white
                , Elevation.e2
                ]
                [ filterMenu model 7
                , restaurantGrid
                ]
            ]


filterRestaurants : Model -> List RestaurantPreview -> List RestaurantPreview
filterRestaurants { priceFilter, includeOnlyOpenRestaurants } =
    List.filter
        (\r ->
            let
                priceLevel =
                    Maybe.withDefault -1 (r.priceLevel)

                restaurantIsOpenNow =
                    Maybe.withDefault False r.openNow

                isCasual =
                    0 <= priceLevel && priceLevel <= 2
            in
                case priceFilter of
                    IncludeBoth ->
                        if includeOnlyOpenRestaurants then
                            restaurantIsOpenNow
                        else
                            True

                    IncludeJustFancy ->
                        if includeOnlyOpenRestaurants then
                            (not isCasual) && restaurantIsOpenNow
                        else
                            not isCasual

                    IncludeJustCasual ->
                        if includeOnlyOpenRestaurants then
                            isCasual && restaurantIsOpenNow
                        else
                            isCasual
        )


filterMenu : Model -> Int -> Html Msg
filterMenu model idNumber =
    let
        { priceFilter, includeOnlyOpenRestaurants } =
            model

        ( includeCasualInSearch, includeFancyInSearch ) =
            case priceFilter of
                IncludeBoth ->
                    ( True, True )

                IncludeJustFancy ->
                    ( False, True )

                IncludeJustCasual ->
                    ( True, False )

        menuItems =
            [ ( includeCasualInSearch, ToggleCasual, "Casual" )
            , ( includeFancyInSearch, ToggleFancy, "Fancy" )
            , ( includeOnlyOpenRestaurants, ToggleIncludeOnlyOpenRestaurants, "Open Now" )
            ]

        menu =
            if model.menuOpen then
                [ ul
                    [ css "position" "absolute"
                    , css "top" "18px"
                    , css "left" "-119px"
                    , css "z-index" "10"
                    , Elevation.e2
                    , Color.background Color.white
                    , Options.attribute <| Attr.attribute "role" "presentation"
                    ]
                    (List.indexedMap menuItem menuItems)
                ]
            else
                []

        menuItem i ( value, msg, text_ ) =
            li
                ([ Options.id << String.join "" << String.split " " <| text_
                 , css "height" "48px"
                 , css "padding" "0 16px"
                 , css "opacity" "1"
                 , css "align-items" "center"
                 , css "display" "flex"
                 , Options.attribute <| Events.onClick msg
                 , Options.attribute <| Events.onWithOptions "keydown" options (dec msg)
                 , Options.attribute <| Events.onMouseEnter <| MouseEnterMenuItem (Just i)
                 , Options.attribute <| Events.onMouseLeave <| MouseEnterMenuItem Nothing
                 , Options.attribute <| Attr.tabindex 0
                 , Options.attribute <| Events.onFocus <| MouseEnterMenuItem (Just i)
                 , Options.attribute <| Events.onBlur <| MouseEnterMenuItem Nothing
                 , Options.attribute <| Attr.attribute "role" "menuitemcheckbox"
                 , Options.attribute <| Attr.attribute "aria-checked" (toString value)
                 ]
                    |> active i model.indexOfMousedMenuItem
                )
                [ checkmark value
                , text text_
                ]

        active i maybeIndex attributes =
            case maybeIndex of
                Just k ->
                    if i == k then
                        (Color.background <| Color.color Color.Grey Color.S100) :: attributes
                    else
                        attributes

                Nothing ->
                    attributes

        checkmark v =
            if v then
                Icon.view "check" [ css "width" "40px" ]
            else
                Options.span [ css "width" "40px" ] []

        ariaActiveDescendant attributes =
            case model.indexOfMousedMenuItem of
                Nothing ->
                    attributes

                Just i ->
                    if i == 0 then
                        (Options.attribute <| Attr.attribute "aria-activedescendant" "Casual") :: attributes
                    else if i == 1 then
                        (Options.attribute <| Attr.attribute "aria-activedescendant" "Fancy") :: attributes
                    else
                        (Options.attribute <| Attr.attribute "aria-activedescendant" "OpenNow") :: attributes

        options =
            { preventDefault = True, stopPropagation = False }

        dec msg =
            let
                tagger code =
                    if code == 32 then
                        Ok msg
                    else
                        Err "not handling that key"
            in
                Json.map tagger Events.keyCode
                    |> Json.andThen fromResult

        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason
    in
        (Options.styled_ div)
            ([ css "position" "relative"
             , css "float" "right"
             , Color.background Color.white
             , Options.attribute <| Attr.attribute "role" "menu"
             ]
                |> ariaActiveDescendant
            )
            []
            ([ Button.render Mdl
                [ idNumber ]
                model.mdl
                [ Button.icon
                , Button.ripple
                , Button.type_ "menu"
                , Button.onClick ToggleMenu
                ]
                [ Icon.i "list"
                ]
             ]
                ++ menu
            )


restaurantCard : Model -> Int -> RestaurantPreview -> Html Msg
restaurantCard { indexOfElevatedCard } i r =
    let
        { name, types, address } =
            r

        elevation =
            case indexOfElevatedCard of
                Nothing ->
                    Elevation.e2

                Just k ->
                    if i == k then
                        Elevation.e6
                    else
                        Elevation.e2

        zIndex =
            case indexOfElevatedCard of
                Nothing ->
                    "0"

                Just k ->
                    if i == k then
                        "3"
                    else
                        "0"

        options =
            { preventDefault = True, stopPropagation = False }

        enterKeyDecoder =
            let
                tagger code =
                    if code == 13 then
                        Ok (OnRestaurantClick r)
                    else
                        Err "not handling that key"
            in
                Json.map tagger Events.keyCode
                    |> Json.andThen fromResult

        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason
    in
        Card.view
            [ css "height" "138px"
            , css "width" "100%"
            , css "border-radius" "0px"
            , css "z-index" zIndex
            , Color.background Color.primary
            , Elevation.transition 250
            , elevation
            , Options.attribute <| Events.onMouseEnter <| MouseEnterRestaurantCard (Just i)
            , Options.attribute <| Events.onMouseLeave <| MouseEnterRestaurantCard Nothing
            , Options.attribute <| Events.onClick <| OnRestaurantClick r
            , Options.attribute <| Attr.tabindex 0
            , Options.attribute <| Events.onFocus <| MouseEnterRestaurantCard (Just i)
            , Options.attribute <| Events.onBlur <| MouseEnterRestaurantCard Nothing
            , Options.attribute <| Events.onWithOptions "keydown" options enterKeyDecoder
            ]
            [ Card.title
                [ Options.attribute <| Attr.attribute "role" "listitem"
                ]
                [ Card.head
                    [ Color.text Color.white
                    , Options.attribute <| Attr.attribute "role" "link"
                    ]
                    [ text name ]
                , Card.subhead
                    [ Color.text Color.white ]
                    [ text address ]
                ]
            ]
