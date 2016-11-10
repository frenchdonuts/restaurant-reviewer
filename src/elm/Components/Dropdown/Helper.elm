module Components.Dropdown.Helper exposing (CyclicIterator, cyclicIterator, currentIndex, inc, dec, jump, next, prev)


type CyclicIterator
    = Iterator
        { min_ : Int
        , max_ : Int
        , currentIndex : Int
        }


cyclicIterator : Int -> Int -> Int -> CyclicIterator
cyclicIterator min_ max_ startingIndex =
    Iterator
        { min_ = min_
        , max_ = max_
        , currentIndex = startingIndex
        }


currentIndex : CyclicIterator -> Int
currentIndex (Iterator iterator) =
    iterator.currentIndex


inc : CyclicIterator -> Int
inc =
    next >> currentIndex


dec : CyclicIterator -> Int
dec =
    prev >> currentIndex


jump : Int -> CyclicIterator -> CyclicIterator
jump newIndex (Iterator iterator) =
    let
        { min_, max_ } =
            iterator

        validatedNewIndex =
            if min_ <= newIndex && newIndex <= max_ then
                newIndex
            else if (abs newIndex - min_) < (abs newIndex - max_) then
                min_
            else
                max_
    in
        Iterator
            { iterator | currentIndex = validatedNewIndex }


next : CyclicIterator -> CyclicIterator
next (Iterator iterator) =
    let
        { min_, max_, currentIndex } =
            iterator

        currentIndex' =
            if (currentIndex >= max_) then
                min_
            else
                currentIndex + 1
    in
        Iterator { iterator | currentIndex = currentIndex' }


prev : CyclicIterator -> CyclicIterator
prev (Iterator iterator) =
    let
        { min_, max_, currentIndex } =
            iterator

        currentIndex' =
            if (currentIndex <= min_) then
                max_
            else
                currentIndex - 1
    in
        Iterator { iterator | currentIndex = currentIndex' }
