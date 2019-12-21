module Queue exposing
    ( Queue
    , empty, singleton, fromList, repeat, range
    , head, tail, take, drop, partition, unzip, toList
    , enqueue, dequeue
    , length, isEmpty, isEqual, any, all, member, maximum, minimum, sum, product
    , map, indexedMap, foldr, foldl, filter, filterMap, reverse
    , append, concat, concatMap, intersperse, map2, map3, map4, map5
    , sort, sortBy, sortWith
    )

{-| Queue FIFO (first-in first-out) data structure.

It has the same API as [`List`](https://package.elm-lang.org/packages/elm/core/latest/List)
and it takes constant time `O(1)` for `enqueue`, `head` and `length` operations.
It takes constant time in average case for `dequeue` `θ(1)`.

@docs Queue


# Construct

@docs empty, singleton, fromList, repeat, range


# Deconstruct

@docs head, tail, take, drop, partition, unzip, toList


# Manipulation

@docs enqueue, dequeue


# Query

@docs length, isEmpty, isEqual, any, all, member, maximum, minimum, sum, product


# Transform

@docs map, indexedMap, foldr, foldl, filter, filterMap, reverse


# Combine

@docs append, concat, concatMap, intersperse, map2, map3, map4, map5


# Sort

@docs sort, sortBy, sortWith

-}


{-| A queue of values.
You can think about a Queue representation like about List,
where left side is the input and right is output:

    fromList   [ 2, 4, 6, 8 ]
    -- indx -> [ 3, 2, 1, 0 ] ->

-}
type Queue a
    = Empty
    | Queue a Int Int (List a) (List a)



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
    Queue element 0 0 [] []


{-| Create a queue from `List`.
-}
fromList : List a -> Queue a
fromList list =
    case List.foldl fromListReverser ( 0, [] ) list of
        ( size, peek :: output ) ->
            Queue peek 0 (size - 1) [] output

        _ ->
            Empty


fromListReverser : a -> ( Int, List a ) -> ( Int, List a )
fromListReverser el ( count, acc ) =
    ( count + 1, el :: acc )


{-| Create a queue with _n_ copies of a value:

    repeat 3 "hi"
        == fromList [ "hi", "hi", "hi" ]

-}
repeat : Int -> a -> Queue a
repeat n value =
    if n < 1 then
        Empty

    else
        value
            |> List.repeat (n - 1)
            |> Queue value 0 (n - 1) []


{-| Create a queue of numbers, every element increasing one.
You give the lowest and the highest number that should be in the queue.

    range 3 6 == fromList [ 6, 5, 4, 3 ]

    range 3 3 == fromList [ 3 ]

    range 6 3 == fromList []

    head (range 3 6) == Just 3

-}
range : Int -> Int -> Queue Int
range lo hi =
    if lo > hi then
        Empty

    else
        Queue lo 0 (hi - lo) [] (List.range (lo + 1) hi)



-- D E C O N S T R U C T


{-| Extract the next element of a queue:

    head empty == Nothing

    head (singleton 0) == Just 0

    head (fromList [ 1, 2, 3 ])
        == Just 3

It takes constant time `O(1)`.

-}
head : Queue a -> Maybe a
head queue =
    case queue of
        Empty ->
            Nothing

        Queue peek _ _ _ _ ->
            Just peek


{-| Extract the rest of the list:

    tail (fromList [ 1, 2, 3 ])
        == Just [ 1, 2 ]

    tail empty == Nothing

-}
tail : Queue a -> Maybe (Queue a)
tail queue =
    case dequeue queue of
        ( Nothing, _ ) ->
            Nothing

        ( _, tailQueue ) ->
            Just tailQueue


{-| Take the first `n` members of a queue:

    take 2 (fromList [ 1, 2, 3 ])
        == formList [ 2, 3 ]

-}
take : Int -> Queue a -> Queue a
take n queue =
    case queue of
        Empty ->
            Empty

        Queue peek sizeIn sizeOut input output ->
            if n <= 0 then
                Empty

            else if n == 1 then
                Queue peek 0 0 [] []

            else if n < 1 + sizeOut then
                Queue peek 0 (n - 1) [] (List.take (n - 1) output)

            else if n == 1 + sizeOut then
                Queue peek 0 sizeOut [] output

            else if n < 1 + sizeOut + sizeIn then
                Queue peek
                    (n - 1 - sizeOut)
                    sizeOut
                    (List.drop (1 + sizeIn + sizeOut - n) input)
                    output

            else
                queue


{-| Drop the first `n` members of a queue:

    drop 2 (fromList [ 1, 2, 3 ])
        == formList [ 1 ]

-}
drop : Int -> Queue a -> Queue a
drop n queue =
    case queue of
        Empty ->
            Empty

        Queue _ sizeIn sizeOut input output ->
            if n <= 0 then
                queue

            else if n < 1 + sizeOut then
                case List.drop (n - 1) output of
                    [] ->
                        Empty

                    nextPeek :: nextOutput ->
                        Queue nextPeek sizeIn (sizeOut - n) input nextOutput

            else if n == 1 + sizeOut then
                fromList input

            else if n < 1 + sizeOut + sizeIn then
                fromList (List.take (1 + sizeIn + sizeOut - n) input)

            else
                Empty


{-| Partition a queue based on some test.
The first queue contains all values that satisfy the test,
and the second queue contains all the value that do not.

    [ 0, 1, 2, 3, 4, 5 ]
        |> fromList
        |> partition (\x -> x < 3)
        == ( fromList [ 0, 1, 2 ], fromList [ 3, 4, 5 ] )

    [ 0, 1, 2, 3, 4, 5 ]
        |> fromList
        |> partition isEven
        == ( fromList [ 0, 2, 4 ], fromList [ 1, 3, 5 ] )

-}
partition : (a -> Bool) -> Queue a -> ( Queue a, Queue a )
partition test queue =
    foldr (partitionStep test) ( Empty, Empty ) queue


partitionStep : (a -> Bool) -> a -> ( Queue a, Queue a ) -> ( Queue a, Queue a )
partitionStep test element ( trues, falses ) =
    if test element then
        ( enqueue element trues, falses )

    else
        ( trues, enqueue element falses )


{-| Decompose a queue of tuples into a tuple of queues.

    [ ( 0, True )
    , ( 17, False )
    , ( 1337, True )
    ]
        |> fromList
        |> unzip
        == ( fromList [ 0, 17, 1337 ]
           , fromList [ True, False, True ]
           )

-}
unzip : Queue ( a, b ) -> ( Queue a, Queue b )
unzip queue =
    foldr unzipStep ( Empty, Empty ) queue


unzipStep : ( a, b ) -> ( Queue a, Queue b ) -> ( Queue a, Queue b )
unzipStep ( a, b ) ( aQueue, bQueue ) =
    ( enqueue a aQueue, enqueue b bQueue )


{-| Convert a queue (FIFO) to list (LIFO):

    toList (fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ]

    empty
        |> enqueue 3
        |> enqueue 2
        |> enqueue 1
        |> toList
        == [ 1, 2, 3 ]

-}
toList : Queue a -> List a
toList queue =
    foldr (::) [] queue



-- M A N I P U L A T E


{-| Add an element to the queue.

    enqueue 1 empty == fromList [ 1 ]

    enqueue 1 (fromList [ 2, 3, 4 ])
        == fromList [ 1, 2, 3, 4 ]

    empty
        |> enqueue 1
        |> enqueue 2
        |> enqueue 3
        == fromList [ 3, 2, 1 ]

It takes constant time `O(1)`.

-}
enqueue : a -> Queue a -> Queue a
enqueue element queue =
    case queue of
        Empty ->
            Queue element 0 0 [] []

        Queue peek sizeIn sizeOut input output ->
            Queue peek (sizeIn + 1) sizeOut (element :: input) output


{-| Extract and remove the first element from the queue:

    dequeue empty == ( Nothing, empty )

    dequeue (singleton 1) == ( Just 1, empty )

    dequeue (fromList [ 1, 2, 3 ])
        == ( Just 3, fromList [ 1, 2 ] )

It takes constant time in average case `θ(1)` (`Ω(1)` and `O(n)`).

-}
dequeue : Queue a -> ( Maybe a, Queue a )
dequeue queue =
    case queue of
        Empty ->
            ( Nothing, Empty )

        Queue peek sizeIn _ input [] ->
            ( Just peek
            , case List.reverse input of
                [] ->
                    Empty

                nextPeek :: nextOutStack ->
                    Queue nextPeek 0 (sizeIn - 1) [] nextOutStack
            )

        Queue peek sizeIn sizeOut input (nextPeek :: nextOutStack) ->
            ( Just peek
            , Queue nextPeek sizeIn (sizeOut - 1) input nextOutStack
            )



-- Q U E R Y


{-| Determine the length of a queue:

    length empty == 0

    length (fromList [ 3, 2, 1 ]) == 3

It takes constant time `O(1)`.

-}
length : Queue a -> Int
length queue =
    case queue of
        Empty ->
            0

        Queue _ sizeIn sizeOut _ _ ->
            sizeIn + sizeOut + 1


{-| Determine if a queue is empty.

    isEmpty (fromList []) == True

    isEmpty (fromList [ 1, 2, 3 ]) == False

-}
isEmpty : Queue a -> Bool
isEmpty =
    (==) Empty


{-| Determine if two queues are equal.
It takes constant time `O(1)` when lengths are different.
-}
isEqual : Queue a -> Queue a -> Bool
isEqual left right =
    case ( left, right ) of
        ( Empty, Empty ) ->
            True

        ( Queue lPeek lSizeIn lSizeOut lInput lOutput, Queue rPeek rSizeIn rSizeOut rInput rOutput ) ->
            if lSizeIn + lSizeOut /= rSizeIn + rSizeOut || lPeek /= rPeek then
                False

            else if lSizeIn == rSizeIn then
                lInput == rInput && lOutput == rOutput

            else
                lInput ++ List.reverse lOutput == rInput ++ List.reverse rOutput

        _ ->
            False


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

        Queue peek _ _ input output ->
            -- @see https://github.com/elm/core/blob/1.0.4/src/List.elm#L291
            if check peek then
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

        Queue peek _ _ input output ->
            List.foldl max (List.foldl max peek output) input
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

        Queue peek _ _ input output ->
            List.foldl min (List.foldl min peek output) input
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

        Queue peek _ _ input output ->
            List.foldl (+) (List.foldl (+) peek output) input


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

        Queue peek _ _ input output ->
            List.foldl (*) (List.foldl (*) peek output) input



-- T R A N S F O R M


{-| Apply a function to every element of a queue:

    map sqrt (fromList [ 1, 4, 9 ])
        == fromList [ 1, 2, 3 ]

-}
map : (a -> b) -> Queue a -> Queue b
map fn queue =
    case queue of
        Empty ->
            Empty

        Queue peek sizeIn sizeOut input output ->
            List.foldr
                ((::) << fn)
                (List.foldl ((::) << fn) [] input)
                output
                |> Queue (fn peek) 0 (sizeIn + sizeOut) []


{-| Same as map but the function is also applied to the index of each element (starting at zero):

    indexedMap Tuple.pair (fromList [ "A", "B", "C" ])
        == fromList [ ( 2, "A" ), ( 1, "B" ), ( 0, "C" ) ]

-}
indexedMap : (Int -> a -> b) -> Queue a -> Queue b
indexedMap fn queue =
    case queue of
        Empty ->
            Empty

        Queue peek sizeIn sizeOut input output ->
            List.foldr
                (indexedMapStep fn)
                (List.foldl (indexedMapStep fn) ( sizeIn + sizeOut, [] ) input)
                output
                |> Tuple.second
                |> Queue (fn 0 peek) 0 (sizeIn + sizeOut) []


indexedMapStep : (Int -> a -> b) -> a -> ( Int, List b ) -> ( Int, List b )
indexedMapStep fn element ( index, list ) =
    ( index - 1
    , fn index element :: list
    )


{-| Reduce queue from left to right (or from the oldest to newest):

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
        == [ 1, 2, 3 ]

So `foldr step state (fromList [ 1, 2, 3 ])` is like saying:

    state
        |> step 3
        |> step 2
        |> step 1

-}
foldr : (a -> b -> b) -> b -> Queue a -> b
foldr fn acc queue =
    case queue of
        Empty ->
            acc

        Queue peek _ _ input output ->
            List.foldr fn
                (List.foldl fn (fn peek acc) output)
                input


{-| Reduce queue from right to left (or from the newest to oldest):

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
        == [ 3, 2, 1 ]

So `foldl step state (fromList [ 1, 2, 3 ])` is like saying:

    state
        |> step 1
        |> step 2
        |> step 3

-}
foldl : (a -> b -> b) -> b -> Queue a -> b
foldl fn acc queue =
    case queue of
        Empty ->
            acc

        Queue peek _ _ input output ->
            List.foldr fn
                (List.foldl fn acc input)
                output
                |> fn peek


{-| Keep elements that satisfy the test:

    filter isEven (fromList [ 1, 2, 3, 4, 5, 6 ])
        == fromList [ 2, 4, 6 ]

-}
filter : (a -> Bool) -> Queue a -> Queue a
filter fn queue =
    case foldl (filterStep fn) ( 0, [] ) queue of
        ( size, peek :: output ) ->
            Queue peek 0 (size - 1) [] output

        _ ->
            Empty


filterStep : (a -> Bool) -> a -> ( Int, List a ) -> ( Int, List a )
filterStep fn element (( size, list ) as acc) =
    if fn element then
        ( size + 1, element :: list )

    else
        acc


{-| Filter out certain values.
For example, maybe you have a bunch of strings from an untrusted source
and you want to turn them into numbers:

    [ "3", "hi", "12", "4th", "May" ]
        |> fromList
        |> filterMap String.toInt
        == fromList [ 3, 12 ]

-}
filterMap : (a -> Maybe b) -> Queue a -> Queue b
filterMap fn queue =
    case foldl (filterMapStep fn) ( 0, [] ) queue of
        ( size, peek :: output ) ->
            Queue peek 0 (size - 1) [] output

        _ ->
            Empty


filterMapStep : (a -> Maybe b) -> a -> ( Int, List b ) -> ( Int, List b )
filterMapStep fn element (( size, list ) as acc) =
    case fn element of
        Nothing ->
            acc

        Just nextElement ->
            ( size + 1, nextElement :: list )


{-| Reverse the queue:

    reverse (fromList [ 1, 2, 3, 4 ])
        == fromList [ 4, 3, 2, 1 ]

-}
reverse : Queue a -> Queue a
reverse queue =
    case queue of
        Empty ->
            Empty

        Queue peek sizeIn sizeOut input output ->
            case input of
                [] ->
                    fromList (peek :: output)

                nextPeek :: nextOutput ->
                    Queue nextPeek (sizeOut + 1) (sizeIn - 1) (peek :: output) nextOutput



-- C O M B I N E


{-| Put two queues together:

    append (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ])
        == fromList [ 1, 1, 2, 3, 5, 8 ]

    append (fromList [ 'a', 'b' ]) (fromList [ 'c' ])
        == fromList [ 'a', 'b', 'c' ]

-}
append : Queue a -> Queue a -> Queue a
append left right =
    case ( left, right ) of
        ( Empty, _ ) ->
            right

        ( _, Empty ) ->
            left

        ( Queue lPeek lSizeIn lSizeOut lInput lOutput, Queue rPeek rSizeIn rSizeOut rInput rOutput ) ->
            Queue
                rPeek
                (lSizeIn + lSizeOut + 1 + rSizeIn)
                rSizeOut
                (lInput ++ List.foldl (::) (lPeek :: rInput) lOutput)
                rOutput


{-| Concatenate a bunch of queues into a single queue:

    [ fromList [ 1, 2 ]
    , fromList [ 3 ]
    , fromList [ 4, 5 ]
    ]
        |> fromList
        |> concat
        == fromList [ 1, 2, 3, 4, 5 ]

-}
concat : Queue (Queue a) -> Queue a
concat queueOfQueues =
    foldr append empty queueOfQueues


{-| Map a given function onto a queue and flatten the resulting queues:

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> Queue b) -> Queue a -> Queue b
concatMap fn queue =
    foldr (append << fn) empty queue


{-| Places the given value between all members of the given queue.

    intersperse ">" (fromList [ "third", "second", "first" ])
        == fromList [ "third", ">", "second", ">", "first" ]

-}
intersperse : a -> Queue a -> Queue a
intersperse delimiter queue =
    case queue of
        Empty ->
            Empty

        Queue peek sizeIn sizeOut input output ->
            List.foldr
                (\el acc -> delimiter :: el :: acc)
                (List.foldl
                    (\el acc -> delimiter :: el :: acc)
                    []
                    input
                )
                output
                |> Queue peek 0 ((sizeIn + sizeOut) * 2) []


{-| Combine two queues, combining them with the given function.
If one queue is longer, the extra elements are dropped.

    map2 (+)
        (fromList [ 1, 2, 3 ])
        (fromList [ 4, 5, 6 ])
        == fromList [ 5, 7, 9 ]

    map2 Tuple.pair
        (fromList [ "alice", "bob", "chuck" ])
        (fromList [ 2, 5, 7, 8 ])
        == fromList
            [ ( "alice", 5 )
            , ( "bob", 7 )
            , ( "chuck", 8 )
            ]

-}
map2 : (a -> b -> result) -> Queue a -> Queue b -> Queue result
map2 fn a b =
    case ( a, b ) of
        ( Queue aPeek aSizeIn aSizeOut aInput aOutput, Queue bPeek bSizeIn bSizeOut bInput bOutput ) ->
            Queue
                (fn aPeek bPeek)
                0
                (min (aSizeIn + aSizeOut) (bSizeIn + bSizeOut))
                []
                (List.map2 fn
                    (aOutput ++ List.reverse aInput)
                    (bOutput ++ List.reverse bInput)
                )

        _ ->
            Empty


{-| -}
map3 : (a -> b -> c -> result) -> Queue a -> Queue b -> Queue c -> Queue result
map3 fn a b c =
    case ( a, b, c ) of
        ( Queue aP aSI aSO aI aO, Queue bP bSI bSO bI bO, Queue cP cSI cSO cI cO ) ->
            Queue
                (fn aP bP cP)
                0
                (min (aSI + aSO) (min (bSI + bSO) (cSI + cSO)))
                []
                (List.map3 fn
                    (aO ++ List.reverse aI)
                    (bO ++ List.reverse bI)
                    (cO ++ List.reverse cI)
                )

        _ ->
            Empty


{-| -}
map4 : (a -> b -> c -> d -> result) -> Queue a -> Queue b -> Queue c -> Queue d -> Queue result
map4 fn a b c d =
    case ( ( a, b ), ( c, d ) ) of
        ( ( Queue aP aSI aSO aI aO, Queue bP bSI bSO bI bO ), ( Queue cP cSI cSO cI cO, Queue dP dSI dSO dI dO ) ) ->
            Queue
                (fn aP bP cP dP)
                0
                (min (aSI + aSO) (min (bSI + bSO) (min (cSI + cSO) (dSI + dSO))))
                []
                (List.map4 fn
                    (aO ++ List.reverse aI)
                    (bO ++ List.reverse bI)
                    (cO ++ List.reverse cI)
                    (dO ++ List.reverse dI)
                )

        _ ->
            Empty


{-| -}
map5 : (a -> b -> c -> d -> e -> result) -> Queue a -> Queue b -> Queue c -> Queue d -> Queue e -> Queue result
map5 fn a b c d e =
    case ( ( a, b ), ( c, d, e ) ) of
        ( ( Queue aP aSI aSO aI aO, Queue bP bSI bSO bI bO ), ( Queue cP cSI cSO cI cO, Queue dP dSI dSO dI dO, Queue eP eSI eSO eI eO ) ) ->
            Queue
                (fn aP bP cP dP eP)
                0
                (min (aSI + aSO) (min (bSI + bSO) (min (cSI + cSO) (min (dSI + dSO) (eSI + eSO)))))
                []
                (List.map5 fn
                    (aO ++ List.reverse aI)
                    (bO ++ List.reverse bI)
                    (cO ++ List.reverse cI)
                    (dO ++ List.reverse dI)
                    (eO ++ List.reverse eI)
                )

        _ ->
            Empty



-- S O R T


{-| Sort values from lowest to highest:

    sort (fromList [ 3, 1, 5 ]) == fromList [ 5, 3, 1 ]

-}
sort : Queue comparable -> Queue comparable
sort queue =
    sortBy identity queue


{-| Sort values by a derived property:

    alice =
        { name = "Alice", height = 1.62 }

    bob =
        { name = "Bob", height = 1.85 }

    chuck =
        { name = "Chuck", height = 1.76 }

    [ chuck, alice, bob ]
        |> fromList
        |> sortBy .name
        == fromList [ chuck, bob, alice ]

    [ chuck, alice, bob ]
        |> fromList
        |> sortBy .height
        == fromList [ bob, chuck, alice ]

    [ "cat", "mouse" ]
        |> fromList
        |> sortBy String.length
        == fromList [ "mouse", "cat" ]

-}
sortBy : (a -> comparable) -> Queue a -> Queue a
sortBy toKey queue =
    case queue of
        Empty ->
            Empty

        Queue peek _ _ input output ->
            -- don't care about order here
            List.foldl (::) (peek :: output) input
                |> List.sortWith (sortByComparator toKey)
                |> fromList


sortByComparator : (a -> comparable) -> a -> a -> Order
sortByComparator toKey left right =
    case compare (toKey left) (toKey right) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| Sort values with a custom comparison function:

    flippedComparison a b =
        case compare a b of
            LT -> GT
            EQ -> EQ
            GT -> LT

    [ 5, 4, 3, 2, 1 ]
        |> fromList
        |> sortWith flippedComparison
        == fromList [ 1, 2, 3, 4, 5 ]

-}
sortWith : (a -> a -> Order) -> Queue a -> Queue a
sortWith cmp queue =
    case queue of
        Empty ->
            Empty

        Queue peek _ _ input output ->
            -- don't care about order here
            List.foldl (::) (peek :: output) input
                |> List.sortWith (sortWithComparator cmp)
                |> fromList


sortWithComparator : (a -> a -> Order) -> a -> a -> Order
sortWithComparator cmp left right =
    case cmp left right of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
