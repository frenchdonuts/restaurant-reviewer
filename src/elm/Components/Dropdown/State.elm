module Components.Dropdown.State exposing (init, update, indexOfSelectedItem)

import Components.Dropdown.Types exposing (..)
import Components.Dropdown.Helper exposing (..)
import Material
import Task
import Dom
import Debug


init : Int -> State
init size =
    State
        { selectedItemIndex = 0
        , highlightedItemIndex = 0
        , iterator = cyclicIterator -1 (size - 1) 0
        , dropdownExpanded = False
        , mdl = Material.model
        }


indexOfSelectedItem : State -> Int
indexOfSelectedItem (State s) =
    s.selectedItemIndex


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        (State s) =
            state

        { selectedItemIndex, highlightedItemIndex, iterator, dropdownExpanded } =
            s

        ( updatedState, cmd ) =
            case msg of
                NoOp ->
                    ( s, Cmd.none )

                HeaderClicked ->
                    ( { s | dropdownExpanded = not dropdownExpanded }, Cmd.none )

                OnEnterKeyPressed ->
                    ( { s
                        | selectedItemIndex = highlightedItemIndex
                        , dropdownExpanded = False
                      }
                    , Cmd.none
                    )

                OnItemClicked index ->
                    ( { s
                        | selectedItemIndex = index
                        , dropdownExpanded = False
                      }
                    , Cmd.none
                    )

                OnMouseOverItem index ->
                    ( { s
                        | highlightedItemIndex = index
                        , iterator = jump index iterator
                      }
                    , Cmd.none
                    )

                OnDownArrow ->
                    let
                        ( State updatedState, cmd ) =
                            onVerticalArrows next state
                    in
                        ( updatedState, Debug.log "focusCmd" cmd )

                OnUpArrow ->
                    let
                        ( State updatedState, cmd ) =
                            onVerticalArrows prev state
                    in
                        ( updatedState, Debug.log "focusCmd" cmd )

                OnFocus ->
                    ( { s
                        | dropdownExpanded = True
                      }
                    , Cmd.none
                    )

                OnBlur ->
                    ( { s
                        | dropdownExpanded = False
                      }
                    , Cmd.none
                    )

                Mdl message' ->
                    Material.update message' s

                DomEffectErr err ->
                    let
                        error =
                            Debug.log "focus error" err
                    in
                        ( s, Cmd.none )

                DomEffectSuc _ ->
                    ( s, Cmd.none )
    in
        ( State updatedState, cmd )


onVerticalArrows : (CyclicIterator -> CyclicIterator) -> State -> ( State, Cmd Msg )
onVerticalArrows cycleFn (State s) =
    let
        { iterator, highlightedItemIndex } =
            s

        iterator' =
            cycleFn iterator

        highlightedItemIndex' =
            currentIndex iterator'
    in
        ( State
            { s
                | dropdownExpanded = True
                , highlightedItemIndex = Debug.log "highlightedItemIndex'" highlightedItemIndex'
                , iterator = iterator'
            }
        , Task.perform DomEffectErr DomEffectSuc (Dom.focus <| "option" ++ (highlightedItemIndex |> toString))
        )
