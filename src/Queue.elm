module Queue exposing
    ( Queue
    , empty, singleton, fromList, repeat, range
    , enqueue, dequeue
    , peek, length, any, all, member, maximum, minimum, sum, product
    , map, indexedMap, foldl, foldr, filter, filterMap, reverse
    , append, concat, concatMap, intersperse, map2
    , toList
    )

{-| Queue FIFO (first-in first-out) data structure.

@docs Queue


# Construction

@docs empty, singleton, fromList, repeat, range


# Manipulation

@docs enqueue, dequeue


# Query

@docs peek, length, any, all, member, maximum, minimum, sum, product


# Transform

@docs map, indexedMap, foldl, foldr, filter, filterMap, reverse


# Combine

@docs append, concat, concatMap, intersperse, map2

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


{-| Determine if any elements satisfy some test:

    any isEven (fromList [ 2, 3 ]) == True

    any isEven (fromList [ 1, 3 ]) == False

    any isEven (fromList []) == False

-}
any : (a -> Bool) -> Queue a -> Bool
any check queue =
    case queue of
        Empty ->
            False

        Queue _ head input output ->
            -- @see https://github.com/elm/core/blob/1.0.4/src/List.elm#L291
            if check head then
                True

            else if List.any check input then
                True

            else
                List.any check output


{-| Determine if all elements satisfy some test.

    all isEven (fromList [ 2, 4 ]) == True

    all isEven (fromList [ 2, 3 ]) == False

    all isEven (fromList []) == True

-}
all : (a -> Bool) -> Queue a -> Bool
all check queue =
    not (any (not << check) queue)


{-| Figure out whether a queue contains a value.

    member 9 (fromList []) == False

    member 9 (fromList [ 1, 2, 3, 4 ]) == False

    member 4 (fromList [ 1, 2, 3, 4 ]) == True

-}
member : a -> Queue a -> Bool
member element queue =
    any ((==) element) queue


{-| Find the maximum element in a non-empty queue:

    maximum (fromList [ 1, 4, 2 ]) == Just 4

    maximum (fromList []) == Nothing

-}
maximum : Queue comparable -> Maybe comparable
maximum queue =
    case queue of
        Empty ->
            Nothing

        Queue _ head input output ->
            List.foldl max (List.foldl max head output) input
                |> Just


{-| Find the minimum element in a non-empty queue:

    minimum (fromList [ 3, 2, 1 ]) == Just 1

    minimum (fromList []) == Nothing

-}
minimum : Queue comparable -> Maybe comparable
minimum queue =
    case queue of
        Empty ->
            Nothing

        Queue _ head input output ->
            List.foldl min (List.foldl min head output) input
                |> Just


{-| Get the sum of the queue elements:

    sum (fromList [ 1, 2, 3 ]) == 6

    sum (fromList [ 1, 1, 1 ]) == 3

    sum (fromList []) == 0

-}
sum : Queue number -> number
sum queue =
    case queue of
        Empty ->
            0

        Queue _ head input output ->
            List.foldl (+) (List.foldl (+) head output) input


{-| Get the product of the queue elements:

    product (from List [ 2, 2, 2 ]) == 8

    product (from List [ 3, 3, 3 ]) == 27

    product (from List []) == 1

-}
product : Queue number -> number
product queue =
    case queue of
        Empty ->
            1

        Queue _ head input output ->
            List.foldl (*) (List.foldl (*) head output) input



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


{-| Reverse the queue:

    reverse (fromList [ 1, 2, 3, 4 ]) == fromList [ 4, 3, 2, 1 ]

-}
reverse : Queue a -> Queue a
reverse queue =
    case queue of
        Empty ->
            Empty

        Queue size head input output ->
            case input of
                [] ->
                    fromList (head :: output)

                nextHead :: nextOutput ->
                    Queue size nextHead (head :: output) nextOutput



-- C O M B I N E


{-| Put two queues together:

    append (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ]) == fromList [ 1, 1, 2, 3, 5, 8 ]

    append (fromList [ 'a', 'b' ]) (fromList [ 'c' ]) == fromList [ 'a', 'b', 'c' ]

-}
append : Queue a -> Queue a -> Queue a
append left right =
    case ( left, right ) of
        ( Empty, _ ) ->
            right

        ( _, Empty ) ->
            left

        ( Queue lSize lHead lInput lOutput, Queue rSize rHead rInput rOutput ) ->
            Queue (lSize + rSize)
                rHead
                (lInput ++ List.foldl (::) (lHead :: rInput) lOutput)
                rOutput


{-| Concatenate a bunch of queues into a single queue:

    concat
        (fromList
            [ fromList [ 1, 2 ]
            , fromList [ 3 ]
            , fromList [ 4, 5 ]
            ]
        )
        == fromList [ 1, 2, 3, 4, 5 ]

-}
concat : Queue (Queue a) -> Queue a
concat queueOfQueues =
    foldl append empty queueOfQueues


{-| Map a given function onto a queue and flatten the resulting queues:
-}
concatMap : (a -> Queue b) -> Queue a -> Queue b
concatMap fn queue =
    foldl (append << fn) empty queue


{-| Places the given value between all members of the given queue.

    intersperse "on" (fromList [ "turtles", "turtles", "turtles" ])
        == fromList [ "turtles", "on", "turtles", "on", "turtles" ]

-}
intersperse : a -> Queue a -> Queue a
intersperse delimiter queue =
    case queue of
        Empty ->
            Empty

        Queue size head input output ->
            List.foldr
                (\el acc -> delimiter :: el :: acc)
                (List.foldl
                    (\el acc -> delimiter :: el :: acc)
                    []
                    input
                )
                output
                |> Queue (size * 2 - 1) head []


{-| Combine two queues, combining them with the given function.
If one queue is longer, the extra elements are dropped.

    map2 (+) (fromList [ 1, 2, 3 ]) (fromList [ 4, 5, 6 ]) == fromList [ 5, 7, 9 ]

    map2 Tuple.pair (fromList [ "alice", "bob", "chuck" ]) (fromList [ 2, 5, 7, 8 ])
        == fromList [ ( "alice", 5 ), ( "bob", 7 ), ( "chuck", 8 ) ]

-}
map2 : (a -> b -> result) -> Queue a -> Queue b -> Queue result
map2 fn a b =
    case ( a, b ) of
        ( Queue aSize aHead aInput aOutput, Queue bSize bHead bInput bOutput ) ->
            Queue
                (min aSize bSize)
                (fn aHead bHead)
                []
                (List.map2 fn
                    (aOutput ++ List.reverse aInput)
                    (bOutput ++ List.reverse bInput)
                )

        _ ->
            Empty



-- U T I L I T I E S


toList : Queue a -> List a
toList queue =
    foldl (::) [] queue
