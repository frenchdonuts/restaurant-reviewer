module Components.Dropdown.Types exposing (..)

import Components.Dropdown.Helper exposing (..)
import Material
import Dom


{- selectedItemIndex - the currently selected item
   Items become selected by:
       The pressing ENTER while the item is highlighted
           Items are highlighted when:
               User mouseovers it
               User navigates to it using the Up/Down arrows
        Clicking on an item
-}


type State
    = State
        { selectedItemIndex : Int
        , highlightedItemIndex : Int
        , iterator : CyclicIterator
        , dropdownExpanded : Bool
        , mdl : Material.Model
        }


type Msg
    = NoOp
    | HeaderClicked
    | OnEnterKeyPressed
    | OnItemClicked Int
    | OnMouseOverItem Int
    | OnDownArrow
    | OnUpArrow
    | OnFocus
    | OnBlur
    | Mdl (Material.Msg Msg)
    | DomEffectErr Dom.Error
    | DomEffectSuc ()
