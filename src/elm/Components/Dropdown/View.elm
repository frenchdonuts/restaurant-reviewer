module Components.Dropdown.View exposing (root)

import Components.Dropdown.Types exposing (..)
import Components.Dropdown.Helper exposing (currentIndex)
import Html exposing (..)
import Html.Events
import Html.Attributes exposing (style)
import Json.Decode as Json
import Material.List as Lists
import Material.Button as Button
import Material.Options as Options
import Material.Icon as Icon
import Material.Elevation as Elevation
import Debug


root : List String -> State -> Html Msg
root items state =
    let
        (State s) =
            state

        { iterator, dropdownExpanded } =
            s

        tagger code =
            case code of
                13 ->
                    OnEnterKeyPressed

                38 ->
                    OnUpArrow

                40 ->
                    OnDownArrow

                _ ->
                    NoOp

        elevation =
            if s.dropdownExpanded then
                Elevation.e2
            else
                Elevation.e0
    in
        Options.div
            [ Options.attribute <| Html.Attributes.id "option-1"
              -- Styling
            , elevation
            , Options.css "position" "relative"
            , Options.css "outline" "none"
            , Options.attribute <| Html.Events.onMouseDown HeaderClicked
            , Options.attribute (Html.Events.on "keydown" (Json.map tagger Html.Events.keyCode))
            , Options.attribute (Html.Events.onFocus OnFocus)
              --, Options.attribute (Html.Events.onBlur OnBlur)
            , Options.attribute (Html.Attributes.tabindex 0)
              -- Accessibility
            , Options.attribute (Html.Attributes.attribute "role" "listbox")
            , Options.attribute <|
                Html.Attributes.attribute
                    "aria-activedescendent"
                    ("option" ++ (iterator |> currentIndex >> toString))
            ]
            [ header items state
            , Lists.ul
                [ -- Styling
                  Options.css "position" "absolute"
                , Options.css "z-index" "2"
                , Options.css "width" "100%"
                , Options.css "background-color" "white"
                , Options.css "margin-top" "0px"
                , Options.css "padding-top" "0px"
                , Options.css "padding-bottom" "0px"
                , Options.css "max-height" "290px"
                , Options.css "overflow" "scroll"
                , elevation
                  -- Accessibility
                ]
                (list items state)
            ]


header : List String -> State -> Html Msg
header items (State state) =
    let
        { selectedItemIndex, highlightedItemIndex, dropdownExpanded, mdl } =
            state

        selectedItem =
            items
                |> List.drop selectedItemIndex
                >> List.head
                >> Maybe.withDefault ""

        tagger code =
            case code of
                13 ->
                    OnEnterKeyPressed

                38 ->
                    OnUpArrow

                40 ->
                    OnDownArrow

                _ ->
                    NoOp

        openDropdownButton =
            let
                icon =
                    if dropdownExpanded then
                        Icon.i "keyboard_arrow_up"
                    else
                        Icon.i "keyboard_arrow_down"
            in
                Options.div
                    []
                    [ icon ]
    in
        Lists.li
            []
            [ Lists.content
                []
                [ text selectedItem ]
            , openDropdownButton
            ]


list : List String -> State -> List (Html Msg)
list items (State state) =
    let
        { selectedItemIndex, highlightedItemIndex, dropdownExpanded } =
            state

        gone =
            if dropdownExpanded then
                Options.css "display" "flex"
            else
                Options.css "display" "none"

        li i item =
            let
                backgroundColor =
                    if i == highlightedItemIndex then
                        Options.css "background-color" "#f5f5f5"
                    else
                        Options.css "background-color" "white"

                id =
                    if i == highlightedItemIndex then
                        Options.attribute <| Html.Attributes.id "focused"
                    else
                        Options.attribute <| Html.Attributes.id ""
            in
                Lists.li
                    [ gone
                    , Options.attribute <| Html.Events.onClick (OnItemClicked i)
                    , Options.attribute <| Html.Events.onMouseOver (OnMouseOverItem i)
                    , backgroundColor
                    ]
                    [ Lists.content
                        [ Options.attribute <| Html.Attributes.tabindex (-1)
                        , Options.attribute <| Html.Attributes.id ("option" ++ toString i)
                        , Options.css "outline" "none"
                        , Options.attribute (Html.Attributes.attribute "role" "option")
                        ]
                        [ text item ]
                    ]
    in
        List.indexedMap li items
