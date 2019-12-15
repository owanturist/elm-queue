module Queue exposing
    ( Queue
    , empty, singleton, fromList, repeat, range
    , enqueue, dequeue
    , peek
    , toList
    )

{-| Queue FIFO (first-in first-out) data structure.

@docs Queue


# Construction

@docs empty, singleton, fromList, repeat, range


# Manipulation

@docs enqueue, dequeue


# Query

@docs peek

-}


{-| A queue of values.
-}
type Queue a
    = Empty
    | Queue Int (List a) (List a) a



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

Construction takes constant time _O(1)_.

-}
singleton : a -> Queue a
singleton element =
    Queue 1 [] [] element


{-| Create a queue from `List`.

Construction takes linear time proportional to `O(n)`, where `n` is length of the list.

-}
fromList : List a -> Queue a
fromList list =
    case List.foldl fromListReverser ( 0, [] ) list of
        ( size, head :: output ) ->
            Queue size [] output head

        _ ->
            Empty


fromListReverser : a -> ( Int, List a ) -> ( Int, List a )
fromListReverser el ( count, acc ) =
    ( count + 1, el :: acc )


{-| Create a queue with _n_ copies of a value:

    repeat 3 ( 0, 0 ) == fromList [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]

Construction takes linear time proportional to `O(n)`.

-}
repeat : Int -> a -> Queue a
repeat n value =
    if n < 1 then
        Empty

    else
        Queue n [] (List.repeat (n - 1) value) value


{-| Create a queue of numbers, every element increasing one.
You give the lowest and the highest number that should be in the queue.

    range 3 6 == fromList [ 3, 4, 5, 6 ]

    range 3 3 == fromList [ 3 ]

    range 6 3 == fromList []

    peek (range 3 6) == Just 6

Construction takes linear time proportional to `O(n)`, where `n` is a length of the range.

-}
range : Int -> Int -> Queue Int
range lo hi =
    if lo > hi then
        Empty

    else
        Queue (hi - lo + 1) [] (rangeHelp lo hi []) hi


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
            Queue 1 [] [] element

        Queue size input output head ->
            Queue (size + 1) (element :: input) output head


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

        Queue size input [] head ->
            ( Just head
            , case List.reverse input of
                [] ->
                    Empty

                nextHead :: nextOutStack ->
                    Queue (size - 1) [] nextOutStack nextHead
            )

        Queue size input (nextHead :: nextOutStack) head ->
            ( Just head
            , Queue (size - 1) input nextOutStack nextHead
            )



-- Q U E R Y


{-| Extract the next element of a queue:

    peek empty == Nothing

    peek (singleton 0) == Just 0

    peek (fromList 1 2 3) == Just 3

-}
peek : Queue a -> Maybe a
peek queue =
    case queue of
        Empty ->
            Nothing

        Queue _ _ _ head ->
            Just head



-- T R A N S F O R M


map : (a -> b) -> Queue a -> Queue b
map fn queue =
    case queue of
        Empty ->
            Empty

        Queue size input output head ->
            Queue size
                []
                (List.foldl ((::) << fn) (List.map fn output) input)
                (fn head)


indexedMapReducer : (Int -> a -> b) -> a -> ( Int, List b ) -> ( Int, List b )
indexedMapReducer fn element ( index, acc ) =
    ( index + 1
    , fn index element :: acc
    )


indexedMap : (Int -> a -> b) -> Queue a -> Queue b
indexedMap fn queue =
    case queue of
        Empty ->
            Empty

        Queue size input output head ->
            Queue
                size
                []
                (List.foldl
                    (indexedMapReducer fn)
                    (List.foldr (indexedMapReducer fn) ( 1, [] ) output)
                    input
                    |> Tuple.second
                )
                (fn 0 head)


foldReducer : (a -> b -> b) -> a -> b -> b
foldReducer fn element acc =
    fn element acc


foldl : (a -> b -> b) -> b -> Queue a -> b
foldl fn acc queue =
    case queue of
        Empty ->
            acc

        Queue _ input output head ->
            List.foldr
                (foldReducer fn)
                (List.foldl
                    (foldReducer fn)
                    (fn head acc)
                    output
                )
                input


foldr : (a -> b -> b) -> b -> Queue a -> b
foldr fn acc queue =
    case queue of
        Empty ->
            acc

        Queue _ input output head ->
            List.foldl
                (foldReducer fn)
                (List.foldr
                    (foldReducer fn)
                    (fn head acc)
                    output
                )
                input



-- U T I L I T I E S


length : Queue a -> Int
length queue =
    case queue of
        Empty ->
            0

        Queue size _ _ _ ->
            size


member : a -> Queue a -> Bool
member element queue =
    case queue of
        Empty ->
            False

        Queue _ input output head ->
            head == element || List.member element input || List.member element output


all : (a -> Bool) -> Queue a -> Bool
all check queue =
    case queue of
        Empty ->
            True

        Queue _ input output head ->
            check head || List.all check input || List.all check output


any : (a -> Bool) -> Queue a -> Bool
any check queue =
    case queue of
        Empty ->
            False

        Queue _ input output head ->
            check head || List.any check input || List.any check output


toList : Queue a -> List a
toList queue =
    case queue of
        Empty ->
            []

        Queue _ input output head ->
            input ++ List.foldl (::) [ head ] output
