{- Modified from
   https://github.com/thebritican/elm-autocomplete/blob/master/examples/src/AccessibleExample.elm
-}


module Components.Autocomplete exposing (..)

import Autocomplete as Menu
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.Textfield as Textfield
import Material.Options as Options
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
    , textfield : Textfield.Model
    , autocompleteId : String
    , focused : Bool
    }


init : String -> State a
init autocompleteId =
    { autoState = Menu.empty
    , howManyToShow = 5
    , query = ""
    , selectedDatum = Nothing
    , showMenu = False
    , textfield = Textfield.defaultModel
    , autocompleteId = autocompleteId
    , focused = False
    }


getQuery : State a -> String
getQuery =
    .query


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
    | OnBlur
    | Textfield Textfield.Msg
    | NoOp


type alias UpdateConfig msg data =
    { toId : data -> String
    , onSelectChoice : Maybe data -> msg
    }


update : UpdateConfig msg a -> Msg -> State a -> List a -> ( State a, Cmd Msg, Maybe msg )
update config msg state data =
    if not state.focused then
        case msg of
            OnFocus ->
                ( { state | focused = True }, Cmd.none, Nothing )

            _ ->
                ( state, Cmd.none, Nothing )
    else
        case msg of
            SetQuery newQuery ->
                let
                    filteredData =
                        acceptableData newQuery config.toId data

                    showMenu =
                        not << List.isEmpty <| filteredData

                    selectedDatum =
                        if List.length filteredData == 1 then
                            List.head filteredData
                        else
                            Nothing
                in
                    ( { state | query = newQuery, showMenu = showMenu, selectedDatum = selectedDatum }, Cmd.none, Nothing )

            SetAutoState autoMsg ->
                let
                    filteredData =
                        Debug.log "filteredData" <| acceptableData state.query config.toId data

                    howManyToShow =
                        List.length filteredData

                    ( newState, maybeMsg ) =
                        Menu.update (updateConfig config) autoMsg howManyToShow state.autoState filteredData

                    newModel =
                        { state | autoState = newState }
                in
                    case maybeMsg of
                        Nothing ->
                            ( newModel, Cmd.none, Nothing )

                        Just updateMsg ->
                            let
                                log =
                                    Debug.log "updateMsg from Menu subcomponent" updateMsg
                            in
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
                    ( escapedModel, Cmd.none, Nothing )

            Wrap toTop ->
                case state.selectedDatum of
                    Just datum ->
                        update config Reset state data

                    Nothing ->
                        let
                            filteredData =
                                acceptableData state.query config.toId data

                            howManyToShow =
                                List.length filteredData
                        in
                            if toTop then
                                ( { state
                                    | autoState = Menu.resetToLastItem (updateConfig config) filteredData howManyToShow state.autoState
                                    , selectedDatum = List.head <| List.reverse <| filteredData
                                    , showMenu = True
                                  }
                                , Cmd.none
                                , Nothing
                                )
                            else
                                ( { state
                                    | autoState = Menu.resetToFirstItem (updateConfig config) filteredData howManyToShow state.autoState
                                    , selectedDatum = List.head <| filteredData
                                    , showMenu = True
                                  }
                                , Cmd.none
                                , Nothing
                                )

            Reset ->
                ( { state | autoState = Menu.reset (updateConfig config) state.autoState, selectedDatum = Nothing }
                , Cmd.none
                , Nothing
                )

            SelectDatumKeyboard id ->
                let
                    newModel =
                        setQuery state config.toId id data
                            |> resetMenu
                in
                    ( newModel, Cmd.none, Just (config.onSelectChoice newModel.selectedDatum) )

            SelectDatumMouse id ->
                let
                    newModel =
                        setQuery state config.toId id data
                            |> resetMenu
                in
                    ( newModel
                    , Task.perform
                        (\_ -> NoOp)
                        (Dom.focus (state.autocompleteId ++ "-input") |> Task.onError (\err -> Task.succeed ()))
                    , Nothing
                    )

            PreviewDatum id ->
                ( { state
                    | selectedDatum =
                        getDatumAtId data config.toId id
                        --, showMenu = state.focused
                  }
                , Cmd.none
                , Nothing
                )

            OnBlur ->
                let
                    newState =
                        resetMenu state
                in
                    ( { newState | focused = False }, Cmd.none, Just (config.onSelectChoice state.selectedDatum) )

            Textfield msg ->
                ( { state | textfield = Textfield.update msg state.textfield }, Cmd.none, Nothing )

            _ ->
                ( state, Cmd.none, Nothing )


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
    , li : Menu.KeySelected -> Menu.MouseSelected -> Int -> data -> Menu.HtmlDetails Never
    , inputLabel : String
    }


view : ViewConfig a -> State a -> List a -> Html Msg
view config state data =
    let
        howManyToShow =
            List.length <| acceptableData state.query config.toId data

        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            let
                tagger code =
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
            in
                Json.map tagger keyCode
                    |> Json.andThen fromResult

        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason

        menu =
            if state.showMenu then
                [ viewMenu config howManyToShow state data ]
            else
                []

        query =
            case state.selectedDatum of
                Just datum ->
                    config.toId datum

                Nothing ->
                    state.query

        comboxboxAttributes =
            [ attribute "role" "combobox"
            , attribute "aria-expanded" <| String.toLower <| toString state.showMenu
            ]
                |> ariaHasPopup

        ariaHasPopup attributes =
            if state.showMenu then
                (attribute "aria-haspopup" "listbox") :: attributes
            else
                attributes

        labelId =
            config.inputLabel
                |> String.split " "
                |> String.join ""
                |> (++) "-label"

        textboxAttributes =
            [ attribute "role" "textbox"
            , attribute "aria-autocomplete" "list"
            , attribute "aria-labelledby" labelId
            , attribute "aria-multiline" "false"
            ]
                |> ariaActiveDescendant
                |> ariaControls

        ariaActiveDescendant attributes =
            if state.showMenu then
                case state.selectedDatum of
                    Just datum ->
                        (attribute "aria-activedescendant" (descendantId config datum)) :: attributes

                    Nothing ->
                        attributes
            else
                attributes

        ariaControls attributes =
            if state.showMenu then
                attribute "aria-controls" (state.autocompleteId ++ "-list") :: attributes
            else
                attributes
    in
        div (comboxboxAttributes ++ [ style [ ( "position", "relative" ) ] ])
            (List.append
                [ span
                    [ id labelId, hidden True ]
                    [ text config.inputLabel ]
                , Textfield.view Textfield
                    state.textfield
                    [ Options.inner (List.map Options.attribute textboxAttributes)
                    , Textfield.value query
                    , Textfield.onInput SetQuery
                    , Textfield.onFocus OnFocus
                    , Textfield.onBlur OnBlur
                    , Textfield.on "keydown" dec
                    , Textfield.label config.inputLabel
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.css "width" "100%"
                    ]
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


viewMenu : ViewConfig a -> Int -> State a -> List a -> Html Msg
viewMenu config howManyToShow state data =
    Html.map SetAutoState (Menu.view (viewConfig config state.autocompleteId) howManyToShow state.autoState (acceptableData state.query config.toId data))


updateConfig : UpdateConfig msg a -> Menu.UpdateConfig Msg a
updateConfig config =
    Menu.updateConfig
        { toId = config.toId
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewDatum maybeId
                else if code == 13 || code == 9 then
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


descendantId : ViewConfig a -> a -> String
descendantId config datum =
    config.toId datum
        |> String.split " "
        |> String.join ""


viewConfig : ViewConfig a -> String -> Menu.ViewConfig a
viewConfig config autocompleteId =
    Menu.viewConfig
        { toId = descendantId config
        , ul =
            config.ul
                ++ [ id <| autocompleteId ++ "-list"
                   , attribute "role" "listbox"
                   ]
        , li = config.li
        }
