module Zipper1D exposing (..)

import List exposing (..)


type Zipper a
    = Zipper
        { before : List a
        , cur : a
        , after : List a
        , index : Int
        }


zipper before cur after =
    Zipper { before = before, cur = cur, after = after, index = 0 }


current : Zipper a -> a
current (Zipper z) =
    z.cur


forward : Zipper a -> Zipper a
forward (Zipper z) =
    case z.after of
        x :: xs ->
            Zipper { before = z.before ++ [ z.cur ], cur = x, after = xs, index = z.index + 1 }

        [] ->
            Zipper z


backward : Zipper a -> Zipper a
backward (Zipper z) =
    let
        { before, cur, after } =
            z
    in
        case z.before |> reverse >> head of
            Just x ->
                Zipper
                    -- before [1,2,3], cur 4, after [5,6]
                    -- before [1,2], cur 3, after [4,5,6]
                    { before = z.before |> reverse >> myTail >> reverse
                    , cur = x
                    , after = z.cur :: z.after
                    , index = z.index - 1
                    }

            Nothing ->
                Zipper z


goto : a -> Zipper a -> Maybe (Zipper a)
goto a z =
    Maybe.oneOf [ lookForward a z, lookBackward a z ]


lookForward : a -> Zipper a -> Maybe (Zipper a)
lookForward a z =
    if current z == a then
        Just z
    else if atMax z then
        Nothing
    else
        lookForward a (forward z)


lookBackward : a -> Zipper a -> Maybe (Zipper a)
lookBackward a z =
    if current z == a then
        Just z
    else if atMin z then
        Nothing
    else
        lookBackward a (backward z)


index : Zipper a -> Int
index (Zipper z) =
    z.index


atMin : Zipper a -> Bool
atMin (Zipper z) =
    case z.before of
        [] ->
            True

        _ ->
            False


atMax : Zipper a -> Bool
atMax (Zipper z) =
    case z.after of
        [] ->
            True

        _ ->
            False


myTail : List a -> List a
myTail xs =
    case xs of
        x :: xs ->
            xs

        [] ->
            []
