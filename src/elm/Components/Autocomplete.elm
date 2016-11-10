module Components.Autocomplete exposing (..)

import Autocomplete as Menu
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import Json.Decode as Json
import Dom
import Task


subscription : Sub Msg
subscription =
    Sub.map SetAutoState Menu.subscription


type alias State a =
    { autoState : Menu.State
    , howManyToShow : Int
    , query : String
    , selectedDatum : Maybe a
    , showMenu : Bool
    }


init : State a
init =
    { autoState = Menu.empty
    , howManyToShow = 5
    , query = ""
    , selectedDatum = Nothing
    , showMenu = False
    }


getSelectedDatum : State a -> Maybe a
getSelectedDatum =
    .selectedDatum


type Msg
    = SetQuery String
    | SetAutoState Menu.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectDatumKeyboard String
    | SelectDatumMouse String
    | PreviewDatum String
    | OnFocus
    | NoOp


type alias UpdateConfig data =
    { toId : data -> String
    }


update : UpdateConfig a -> Msg -> State a -> List a -> ( State a, Cmd Msg )
update config msg state data =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| (acceptableData newQuery config.toId data)
            in
                { state | query = newQuery, showMenu = showMenu, selectedDatum = Nothing } ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Menu.update (updateConfig config) autoMsg state.howManyToShow state.autoState (acceptableData state.query config.toId data)

                newModel =
                    { state | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update config updateMsg newModel data

        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (acceptableData state.query config.toId data)

                handleEscape =
                    if validOptions then
                        state
                            |> removeSelection
                            |> resetMenu
                    else
                        { state | query = "" }
                            |> removeSelection
                            |> resetMenu

                escapedModel =
                    case state.selectedDatum of
                        Just datum ->
                            if state.query == (config.toId datum) then
                                state
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
                escapedModel ! []

        Wrap toTop ->
            case state.selectedDatum of
                Just person ->
                    update config Reset state data

                Nothing ->
                    if toTop then
                        { state
                            | autoState = Menu.resetToLastItem (updateConfig config) (acceptableData state.query config.toId data) state.howManyToShow state.autoState
                            , selectedDatum = List.head <| List.reverse <| List.take state.howManyToShow <| (acceptableData state.query config.toId data)
                        }
                            ! []
                    else
                        { state
                            | autoState = Menu.resetToFirstItem (updateConfig config) (acceptableData state.query config.toId data) state.howManyToShow state.autoState
                            , selectedDatum = List.head <| List.take state.howManyToShow <| (acceptableData state.query config.toId data)
                        }
                            ! []

        Reset ->
            { state | autoState = Menu.reset (updateConfig config) state.autoState, selectedDatum = Nothing } ! []

        SelectDatumKeyboard id ->
            let
                newModel =
                    setQuery state config.toId id data
                        |> resetMenu
            in
                newModel ! []

        SelectDatumMouse id ->
            let
                newModel =
                    setQuery state config.toId id data
                        |> resetMenu
            in
                ( newModel, Task.perform (\err -> NoOp) (\_ -> NoOp) (Dom.focus "president-input") )

        PreviewDatum id ->
            { state | selectedDatum = getDatumAtId data config.toId id } ! []

        OnFocus ->
            state ! []

        NoOp ->
            state ! []


resetInput : State a -> State a
resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu


removeSelection : State a -> State a
removeSelection model =
    { model | selectedDatum = Nothing }


getDatumAtId : List a -> (a -> String) -> String -> Maybe a
getDatumAtId data toId id =
    List.filter (\datum -> toId datum == id) data
        |> List.head


setQuery : State a -> (a -> String) -> String -> List a -> State a
setQuery state toId id data =
    { state
        | query = Maybe.withDefault "" <| Maybe.map toId <| getDatumAtId data toId id
        , selectedDatum = getDatumAtId data toId id
    }


resetMenu : State a -> State a
resetMenu state =
    { state
        | autoState = Menu.empty
        , showMenu = False
    }


type alias ViewConfig data =
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : Menu.KeySelected -> Menu.MouseSelected -> data -> Menu.HtmlDetails Never
    , inputLabel : String
    }


view : ViewConfig a -> State a -> List a -> Html Msg
view config state data =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.customDecoder keyCode
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
            )

        menu =
            if state.showMenu then
                [ viewMenu config state data ]
            else
                []

        query =
            case state.selectedDatum of
                Just datum ->
                    config.toId datum

                Nothing ->
                    state.query

        activeDescendant attributes =
            case state.selectedDatum of
                Just datum ->
                    (attribute "aria-activedescendant"
                        (config.toId datum)
                    )
                        :: attributes

                Nothing ->
                    attributes
    in
        div [ class "input-field" ]
            (List.append
                [ input
                    (activeDescendant
                        [ onInput SetQuery
                        , onFocus OnFocus
                        , onWithOptions "keydown" options dec
                        , value query
                        , id "president-input"
                        , placeholder "No Preference"
                        , class "validate"
                        , autocomplete False
                        , attribute "aria-owns" "list-of-presidents"
                        , attribute "aria-expanded" <| String.toLower <| toString state.showMenu
                        , attribute "aria-haspopup" <| String.toLower <| toString state.showMenu
                        , attribute "role" "combobox"
                        , attribute "aria-autocomplete" "list"
                        ]
                    )
                    []
                , label [ for "president-input", class "active" ] [ text config.inputLabel ]
                ]
                menu
            )


acceptableData : String -> (a -> String) -> List a -> List a
acceptableData query toId data =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << toId) data


viewMenu : ViewConfig a -> State a -> List a -> Html Msg
viewMenu config state data =
    div [ style [ ( "position", "relative" ) ] ]
        [ Html.map SetAutoState (Menu.view (viewConfig config) state.howManyToShow state.autoState (acceptableData state.query config.toId data)) ]


updateConfig : UpdateConfig a -> Menu.UpdateConfig Msg a
updateConfig config =
    Menu.updateConfig
        { toId = config.toId
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewDatum maybeId
                else if code == 13 then
                    Maybe.map SelectDatumKeyboard maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewDatum id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectDatumMouse id
        , separateSelections = False
        }


viewConfig : ViewConfig a -> Menu.ViewConfig a
viewConfig config =
    Menu.viewConfig
        { toId = config.toId
        , ul = config.ul
        , li = config.li
        }
