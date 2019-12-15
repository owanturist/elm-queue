module Queue exposing
    ( Queue
    , empty, singleton, fromList, repeat, range
    , enqueue, dequeue
    , peek, length
    , map, indexedMap, foldl, foldr, filter, filterMap
    , toList
    )

{-| Queue FIFO (first-in first-out) data structure.

@docs Queue


# Construction

@docs empty, singleton, fromList, repeat, range


# Manipulation

@docs enqueue, dequeue


# Query

@docs peek, length


# Transform

@docs map, indexedMap, foldl, foldr, filter, filterMap

-}


{-| A queue of values.
-}
type Queue a
    = Empty
    | Queue Int a (List a) (List a)



-- C O N S T R U C T I O N


{-| Create an empty queue:

    empty == fromList []

-}
empty : Queue a
empty =
    Empty


{-| Create a queue with only one element:

    singleton 1234 == fromList [ 1234 ]

    singleton "hi" == fromList [ "hi" ]

-}
singleton : a -> Queue a
singleton element =
    Queue 1 element [] []


{-| Create a queue from `List`.

Construction takes linear time proportional to `O(n)`, where `n` is length of the list.

-}
fromList : List a -> Queue a
fromList list =
    case List.foldl fromListReverser ( 0, [] ) list of
        ( size, head :: output ) ->
            Queue size head [] output

        _ ->
            Empty


fromListReverser : a -> ( Int, List a ) -> ( Int, List a )
fromListReverser el ( count, acc ) =
    ( count + 1, el :: acc )


{-| Create a queue with _n_ copies of a value:

    repeat 3 ( 0, 0 ) == fromList [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]

-}
repeat : Int -> a -> Queue a
repeat n value =
    if n < 1 then
        Empty

    else
        value
            |> List.repeat (n - 1)
            |> Queue n value []


{-| Create a queue of numbers, every element increasing one.
You give the lowest and the highest number that should be in the queue.

    range 3 6 == fromList [ 3, 4, 5, 6 ]

    range 3 3 == fromList [ 3 ]

    range 6 3 == fromList []

    peek (range 3 6) == Just 6

-}
range : Int -> Int -> Queue Int
range lo hi =
    if lo > hi then
        Empty

    else
        []
            |> rangeHelp lo hi
            |> Queue (hi - lo + 1) hi []


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi acc =
    if lo < hi then
        rangeHelp (lo + 1) hi (lo :: acc)

    else
        acc



-- M A N I P U L A T E


{-| Add an element to the queue.

    enqueue 1 empty == fromList [ 1 ]

    enqueue 1 (fromList [ 2, 3, 4 ]) == fromList [ 1, 2, 3, 4 ]

    empty |> enqueue 1 |> enqueue 2 |> enqueue 3 == fromList [ 3, 2, 1 ]

It takes constant time `O(1)`.

-}
enqueue : a -> Queue a -> Queue a
enqueue element queue =
    case queue of
        Empty ->
            Queue 1 element [] []

        Queue size head input output ->
            Queue (size + 1) head (element :: input) output


{-| Extract and remove the first element from the queue:

    dequeue empty == ( Nothing, empty )

    dequeue (fromList [ 1 ]) == ( Just 1, empty )

    dequeue (fromList [ 1, 2, 3 ]) == ( Just 3, fromList [ 1, 2 ] )

It takes constant time in average case `θ(1)` (`Ω(1)` and `O(n)` where `n` is size of the queue).

-}
dequeue : Queue a -> ( Maybe a, Queue a )
dequeue queue =
    case queue of
        Empty ->
            ( Nothing, Empty )

        Queue size head input [] ->
            ( Just head
            , case List.reverse input of
                [] ->
                    Empty

                nextHead :: nextOutStack ->
                    Queue (size - 1) nextHead [] nextOutStack
            )

        Queue size head input (nextHead :: nextOutStack) ->
            ( Just head
            , Queue (size - 1) nextHead input nextOutStack
            )



-- Q U E R Y


{-| Determine the length of a queue:

    length empty == 0

    length (fromList [ 3, 2, 1 ]) == 3

-}
length : Queue a -> Int
length queue =
    case queue of
        Empty ->
            0

        Queue size _ _ _ ->
            size


{-| Extract the next element of a queue:

    peek empty == Nothing

    peek (singleton 0) == Just 0

    peek (fromList 1 2 3) == Just 3

It takes constant time `O(1)`.

-}
peek : Queue a -> Maybe a
peek queue =
    case queue of
        Empty ->
            Nothing

        Queue _ head _ _ ->
            Just head



-- T R A N S F O R M


{-| Apply a function to every element of a queue:

    map sqrt (fromList [ 1, 4, 9 ]) == fromList [ 1, 2, 3 ]

It takes linear time proportional to `O(n)` where `n` is size of the queue.

-}
map : (a -> b) -> Queue a -> Queue b
map fn queue =
    case queue of
        Empty ->
            Empty

        Queue size head input output ->
            List.foldr
                ((::) << fn)
                (List.foldl ((::) << fn) [] input)
                output
                |> Queue size (fn head) []


{-| Same as map but the function is also applied to the index of each element (starting at zero):

    indexedMap Tuple.pair (fromList [ "A", "B", "C" ])
        == fromList [ ( 2, "A" ), ( 1, "B" ), ( 0, "C" ) ]

It takes linear time proportional to `O(n)` where `n` is size of the queue.

-}
indexedMap : (Int -> a -> b) -> Queue a -> Queue b
indexedMap fn queue =
    case queue of
        Empty ->
            Empty

        Queue size head input output ->
            List.foldr
                (indexedMapReducer fn)
                (List.foldl (indexedMapReducer fn) ( size - 1, [] ) input)
                output
                |> Tuple.second
                |> Queue size (fn 0 head) []


indexedMapReducer : (Int -> a -> b) -> a -> ( Int, List b ) -> ( Int, List b )
indexedMapReducer fn element ( index, list ) =
    ( index - 1
    , fn index element :: list
    )


{-| Reduce queue from left to right (or from the oldest to newest):

    empty
        |> enqueue 3
        |> enqueue 2
        |> enqueue 1
        |> foldl (+) 0
        === 6

    empty
        |> enqueue 3
        |> enqueue 2
        |> enqueue 1
        |> foldl (::) []
        == [ 1, 2, 3 ]

So `foldl step state (fromList [ 1, 2, 3 ])` is like saying:

    state
        |> step 3
        |> step 2
        |> step 1

-}
foldl : (a -> b -> b) -> b -> Queue a -> b
foldl fn acc queue =
    case queue of
        Empty ->
            acc

        Queue _ head input output ->
            List.foldr fn
                (List.foldl fn (fn head acc) output)
                input


{-| Reduce queue from right to left (or from the newest to oldest):

    empty
        |> enqueue 3
        |> enqueue 2
        |> enqueue 1
        |> foldr (+) 0
        === 6

    empty
        |> enqueue 3
        |> enqueue 2
        |> enqueue 1
        |> foldr (::) []
        == [ 3, 2, 1 ]

So `foldr step state (fromList [ 1, 2, 3 ])` is like saying:

    state
        |> step 1
        |> step 2
        |> step 3

-}
foldr : (a -> b -> b) -> b -> Queue a -> b
foldr fn acc queue =
    case queue of
        Empty ->
            acc

        Queue _ head input output ->
            List.foldr fn
                (List.foldl fn acc input)
                output
                |> fn head


{-| Keep elements that satisfy the test:

    filter isEven (fromList [ 1, 2, 3, 4, 5, 6 ]) == fromList [ 2, 4, 6 ]

-}
filter : (a -> Bool) -> Queue a -> Queue a
filter fn queue =
    case foldr (filterReducer fn) ( 0, [] ) queue of
        ( size, head :: output ) ->
            Queue size head [] output

        _ ->
            Empty


filterReducer : (a -> Bool) -> a -> ( Int, List a ) -> ( Int, List a )
filterReducer fn element (( size, list ) as acc) =
    if fn element then
        ( size + 1, element :: list )

    else
        acc


{-| Filter out certain values.
For example, maybe you have a bunch of strings from an untrusted source
and you want to turn them into numbers:

    filterMap String.toInt (fromList [ "3", "hi", "12", "4th", "May" ])
        == fromList [ 3, 12 ]

-}
filterMap : (a -> Maybe b) -> Queue a -> Queue b
filterMap fn queue =
    case foldr (filterMapReducer fn) ( 0, [] ) queue of
        ( size, head :: output ) ->
            Queue size head [] output

        _ ->
            Empty


filterMapReducer : (a -> Maybe b) -> a -> ( Int, List b ) -> ( Int, List b )
filterMapReducer fn element (( size, list ) as acc) =
    case fn element of
        Nothing ->
            acc

        Just nextElement ->
            ( size + 1, nextElement :: list )



-- U T I L I T I E S


member : a -> Queue a -> Bool
member element queue =
    case queue of
        Empty ->
            False

        Queue _ head input output ->
            head == element || List.member element input || List.member element output


all : (a -> Bool) -> Queue a -> Bool
all check queue =
    case queue of
        Empty ->
            True

        Queue _ head input output ->
            check head || List.all check input || List.all check output


any : (a -> Bool) -> Queue a -> Bool
any check queue =
    case queue of
        Empty ->
            False

        Queue _ head input output ->
            check head || List.any check input || List.any check output


toList : Queue a -> List a
toList queue =
    foldl (::) [] queue
