module QueueTests exposing (..)

import Expect
import Fuzz
import Queue
import Test exposing (Test, describe, fuzz, fuzz2, test)



-- C O N S T R U C T I O N


isEven : Int -> Bool
isEven =
    (==) 0 << modBy 2


emptySuite : Test
emptySuite =
    test "Queue.empty" <|
        \_ ->
            Queue.toList Queue.empty
                |> Expect.equalLists []


singletonSuite : Test
singletonSuite =
    fuzz Fuzz.string "Queue.singleton" <|
        \val ->
            Queue.singleton val
                |> Queue.toList
                |> Expect.equalLists [ val ]


fromListSuite : Test
fromListSuite =
    fuzz (Fuzz.list Fuzz.string) "Queue.fromList" <|
        \list ->
            Queue.fromList list
                |> Queue.toList
                |> Expect.equalLists list


repeatSuite : Test
repeatSuite =
    describe "Queue.repeat"
        [ fuzz Fuzz.string "zero n" <|
            \val ->
                Queue.repeat 0 val
                    |> Queue.toList
                    |> Expect.equalLists (List.repeat 0 val)

        --
        , fuzz2 (Fuzz.intRange 1 100) Fuzz.string "multiple n" <|
            \n val ->
                Queue.repeat n val
                    |> Queue.toList
                    |> Expect.equalLists (List.repeat n val)
        ]


rangeSuite : Test
rangeSuite =
    describe "Queue.range"
        [ fuzz2 (Fuzz.intRange 1 10) (Fuzz.intRange -10 0) "lo > hi" <|
            \lo hi ->
                Queue.range lo hi
                    |> Queue.toList
                    |> Expect.equalLists (List.range lo hi)

        --
        , fuzz Fuzz.int "lo == hi" <|
            \lo ->
                Queue.range lo lo
                    |> Queue.toList
                    |> Expect.equalLists (List.range lo lo)

        --
        , fuzz2 (Fuzz.intRange -10 -1) (Fuzz.intRange 0 10) "lo < hi" <|
            \lo hi ->
                Queue.range lo hi
                    |> Queue.toList
                    |> Expect.equalLists (List.range lo hi)
        ]



-- M A N I P U L A T I O N


enqueueSuite : Test
enqueueSuite =
    describe "Queue.enqueue"
        [ fuzz Fuzz.int "empty" <|
            \val ->
                Queue.empty
                    |> Queue.enqueue val
                    |> Queue.toList
                    |> Expect.equalLists [ val ]

        --
        , fuzz2 Fuzz.int Fuzz.int "singleton" <|
            \first last ->
                Queue.singleton first
                    |> Queue.enqueue last
                    |> Queue.toList
                    |> Expect.equalLists [ last, first ]

        --
        , fuzz2 (Fuzz.intRange 0 4) (Fuzz.intRange 5 10) "fromList" <|
            \first last ->
                Queue.fromList [ -4, -3, -2, -1, first ]
                    |> Queue.enqueue last
                    |> Queue.toList
                    |> Expect.equalLists [ last, -4, -3, -2, -1, first ]

        --
        , fuzz2 Fuzz.string Fuzz.string "repeat" <|
            \val last ->
                Queue.repeat 2 val
                    |> Queue.enqueue last
                    |> Queue.toList
                    |> Expect.equalLists [ last, val, val ]

        --
        , fuzz (Fuzz.intRange 5 10) "range" <|
            \last ->
                Queue.range 1 5
                    |> Queue.enqueue last
                    |> Queue.toList
                    |> Expect.equalLists [ last, 1, 2, 3, 4, 5 ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 0
                    |> Queue.enqueue 1
                    |> Queue.enqueue 2
                    |> Queue.enqueue 3
                    |> Queue.enqueue 4
                    |> Queue.toList
                    |> Expect.equalLists [ 4, 3, 2, 1, 0 ]

        --
        , fuzz (Fuzz.intRange 5 100) "dequeue" <|
            \val ->
                Queue.fromList [ 0, 1, 2, 3, 4 ]
                    |> Queue.dequeue
                    |> Tuple.second
                    |> Queue.enqueue val
                    |> Queue.toList
                    |> Expect.equalLists [ val, 0, 1, 2, 3 ]
        ]


dequeueSuit : Test
dequeueSuit =
    describe "Queue.dequeue"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal Nothing << Tuple.first
                        , Expect.equalLists [] << Queue.toList << Tuple.second
                        ]

        --
        , fuzz Fuzz.int "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal (Just val) << Tuple.first
                        , Expect.equalLists [] << Queue.toList << Tuple.second
                        ]

        --
        , fuzz (Fuzz.intRange 5 10) "fromList" <|
            \val ->
                Queue.fromList [ 0, 1, 2, 3, 4, val ]
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal (Just val) << Tuple.first
                        , Expect.equalLists [ 0, 1, 2, 3, 4 ] << Queue.toList << Tuple.second
                        ]

        --
        , fuzz Fuzz.string "repeat" <|
            \val ->
                Queue.repeat 3 val
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal (Just val) << Tuple.first
                        , Expect.equalLists [ val, val ] << Queue.toList << Tuple.second
                        ]

        --
        , test "range" <|
            \_ ->
                Queue.range 1 5
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal (Just 5) << Tuple.first
                        , Expect.equalLists [ 1, 2, 3, 4 ] << Queue.toList << Tuple.second
                        ]

        --
        , fuzz2 (Fuzz.intRange 5 10) (Fuzz.intRange 11 20) "queue" <|
            \first second ->
                Queue.fromList [ 0, 1, 2, 3, 4, first ]
                    |> Queue.enqueue second
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal (Just first) << Tuple.first
                        , Expect.equalLists [ second, 0, 1, 2, 3, 4 ] << Queue.toList << Tuple.second
                        ]

        --
        , test "dequeue" <|
            \_ ->
                Queue.fromList [ 1, 2, 3 ]
                    |> Queue.dequeue
                    |> Expect.all
                        [ Expect.equal (Just 3) << Tuple.first
                        , Tuple.second
                            >> Queue.dequeue
                            >> Expect.all
                                [ Expect.equal (Just 2) << Tuple.first
                                , Tuple.second
                                    >> Queue.dequeue
                                    >> Expect.all
                                        [ Expect.equal (Just 1) << Tuple.first
                                        , Expect.equal Nothing << Tuple.first << Queue.dequeue << Tuple.second
                                        ]
                                ]
                        ]
        ]



-- Q U E R Y


lengthSuit : Test
lengthSuit =
    describe "Queue.length"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.length
                    |> Expect.equal 0

        --
        , fuzz Fuzz.string "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.length
                    |> Expect.equal 1

        --
        , fuzz (Fuzz.list Fuzz.int) "fromList" <|
            \list ->
                Queue.fromList list
                    |> Queue.length
                    |> Expect.equal (List.length list)

        --
        , fuzz2 (Fuzz.intRange 1 100) Fuzz.string "repeat" <|
            \n val ->
                Queue.repeat n val
                    |> Queue.length
                    |> Expect.equal n

        --
        , fuzz2 (Fuzz.intRange -10 -1) (Fuzz.intRange 0 10) "range" <|
            \lo hi ->
                Queue.range lo hi
                    |> Queue.length
                    |> Expect.equal (hi - lo + 1)

        --
        , fuzz (Fuzz.list (Fuzz.intRange 0 100)) "enqueue" <|
            \list ->
                Queue.fromList list
                    |> Queue.enqueue 0
                    |> Queue.length
                    |> Expect.equal (List.length list + 1)

        --
        , fuzz (Fuzz.list (Fuzz.intRange 0 100)) "fromList + enqueue" <|
            \list ->
                Queue.fromList list
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.length
                    |> Expect.equal (List.length list + 3)

        --
        , test "dequeue" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5 ]
                    |> Queue.dequeue
                    |> Tuple.second
                    |> Queue.length
                    |> Expect.equal 4

        --
        , fuzz (Fuzz.list (Fuzz.intRange 0 100)) "map" <|
            \list ->
                Queue.fromList list
                    |> Queue.map ((*) 2)
                    |> Queue.length
                    |> Expect.equal (List.length list)

        --
        , fuzz (Fuzz.list (Fuzz.intRange 0 100)) "indexedMap" <|
            \list ->
                Queue.fromList list
                    |> Queue.indexedMap Tuple.pair
                    |> Queue.length
                    |> Expect.equal (List.length list)

        --
        , test "filter" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filter ((==) 0 << modBy 2)
                    |> Queue.length
                    |> Expect.equal 3

        --
        , test "filterMap" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filterMap
                        (\el ->
                            if el > 3 then
                                Nothing

                            else
                                Just el
                        )
                    |> Queue.length
                    |> Expect.equal 3

        --
        , fuzz (Fuzz.list Fuzz.int) "reverse" <|
            \list ->
                Queue.fromList list
                    |> Queue.reverse
                    |> Queue.length
                    |> Expect.equal (List.length list)
        ]


peekSuit : Test
peekSuit =
    describe "Queue.peek"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.peek
                    |> Expect.equal Nothing

        --
        , fuzz Fuzz.string "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.peek
                    |> Expect.equal (Just val)

        --
        , fuzz2 Fuzz.int Fuzz.int "fromList" <|
            \first second ->
                Queue.fromList [ first, second ]
                    |> Queue.peek
                    |> Expect.equal (Just second)

        --
        , fuzz2 (Fuzz.intRange 1 100) Fuzz.string "repeat" <|
            \n val ->
                Queue.repeat n val
                    |> Queue.peek
                    |> Expect.equal (Just val)

        --
        , fuzz2 (Fuzz.intRange -10 -1) (Fuzz.intRange 0 10) "range" <|
            \lo hi ->
                Queue.range lo hi
                    |> Queue.peek
                    |> Expect.equal (Just hi)

        --
        , fuzz2 (Fuzz.intRange 0 4) (Fuzz.intRange 5 10) "enqueue" <|
            \first last ->
                Queue.fromList [ -4, -3, -2, -1, first ]
                    |> Queue.enqueue last
                    |> Queue.peek
                    |> Expect.equal (Just first)

        --
        , fuzz (Fuzz.intRange 0 5) "dequeue" <|
            \second ->
                Queue.fromList [ -4, -3, -2, second, -1 ]
                    |> Queue.dequeue
                    |> Tuple.second
                    |> Queue.peek
                    |> Expect.equal (Just second)

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.peek
                    |> Expect.equal (Just 6)

        --
        , fuzz (Fuzz.intRange 6 100) "map" <|
            \val ->
                Queue.fromList [ 1, 2, 3, 4, 5, val ]
                    |> Queue.map ((*) 2)
                    |> Queue.peek
                    |> Expect.equal (Just (val * 2))

        --
        , fuzz (Fuzz.intRange 6 100) "indexedMap" <|
            \val ->
                Queue.fromList [ 1, 2, 3, 4, 5, val ]
                    |> Queue.indexedMap Tuple.pair
                    |> Queue.peek
                    |> Expect.equal (Just ( 0, val ))

        --
        , test "filter" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6, 7 ]
                    |> Queue.filter ((==) 0 << modBy 2)
                    |> Queue.peek
                    |> Expect.equal (Just 6)

        --
        , test "filterMap" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filterMap
                        (\el ->
                            if el > 3 then
                                Nothing

                            else
                                Just el
                        )
                    |> Queue.peek
                    |> Expect.equal (Just 3)

        --
        , fuzz Fuzz.int "reverse" <|
            \val ->
                Queue.fromList [ val, 2, 3, 4, 5, 6 ]
                    |> Queue.reverse
                    |> Queue.peek
                    |> Expect.equal (Just val)
        ]


anySuit : Test
anySuit =
    describe "Queue.any"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.any isEven
                    |> Expect.equal False

        --
        , fuzz Fuzz.int "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.any isEven
                    |> Expect.equal (isEven val)

        --
        , describe "fromList"
            [ test "[ 2, 3 ]" <|
                \_ ->
                    Queue.fromList [ 2, 3 ]
                        |> Queue.any isEven
                        |> Expect.equal True

            --
            , test "[ 1, 3 ]" <|
                \_ ->
                    Queue.fromList [ 1, 3 ]
                        |> Queue.any isEven
                        |> Expect.equal False

            --
            , fuzz (Fuzz.list Fuzz.int) "fuzz" <|
                \list ->
                    Queue.fromList list
                        |> Queue.any isEven
                        |> Expect.equal (List.any isEven list)
            ]

        --
        , describe "enqueue"
            [ test "6 5 4 3 2 1" <|
                \_ ->
                    Queue.empty
                        |> Queue.enqueue 6
                        |> Queue.enqueue 5
                        |> Queue.enqueue 4
                        |> Queue.enqueue 3
                        |> Queue.enqueue 2
                        |> Queue.enqueue 1
                        |> Queue.any isEven
                        |> Expect.equal True

            --
            , test "9 7 5 3 1" <|
                \_ ->
                    Queue.empty
                        |> Queue.enqueue 9
                        |> Queue.enqueue 7
                        |> Queue.enqueue 5
                        |> Queue.enqueue 3
                        |> Queue.enqueue 1
                        |> Queue.any isEven
                        |> Expect.equal False
            ]

        --
        , describe "fromList + enqueue"
            [ test "6 5 4 3 2 1" <|
                \_ ->
                    Queue.fromList [ 4, 5, 6 ]
                        |> Queue.enqueue 3
                        |> Queue.enqueue 2
                        |> Queue.enqueue 1
                        |> Queue.any isEven
                        |> Expect.equal True

            --
            , test "9 7 5 3 1" <|
                \_ ->
                    Queue.fromList [ 9, 7, 5 ]
                        |> Queue.enqueue 3
                        |> Queue.enqueue 1
                        |> Queue.any isEven
                        |> Expect.equal False
            ]

        --
        , fuzz (Fuzz.list Fuzz.int) "reverse" <|
            \list ->
                Queue.fromList list
                    |> Queue.reverse
                    |> Queue.any isEven
                    |> Expect.equal (List.any isEven (List.reverse list))
        ]


allSuit : Test
allSuit =
    describe "Queue.all"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.all isEven
                    |> Expect.equal True

        --
        , fuzz Fuzz.int "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.all isEven
                    |> Expect.equal (isEven val)

        --
        , describe "fromList"
            [ test "[ 2, 4 ]" <|
                \_ ->
                    Queue.fromList [ 2, 4 ]
                        |> Queue.all isEven
                        |> Expect.equal True

            --
            , test "[ 2, 3 ]" <|
                \_ ->
                    Queue.fromList [ 2, 3 ]
                        |> Queue.all isEven
                        |> Expect.equal False

            --
            , fuzz (Fuzz.list Fuzz.int) "fuzz" <|
                \list ->
                    Queue.fromList list
                        |> Queue.all isEven
                        |> Expect.equal (List.all isEven list)
            ]

        --
        , describe "enqueue"
            [ test "6 5 4 3 2 1" <|
                \_ ->
                    Queue.empty
                        |> Queue.enqueue 6
                        |> Queue.enqueue 5
                        |> Queue.enqueue 4
                        |> Queue.enqueue 3
                        |> Queue.enqueue 2
                        |> Queue.enqueue 1
                        |> Queue.all isEven
                        |> Expect.equal False

            --
            , test "10 8 6 4 2" <|
                \_ ->
                    Queue.empty
                        |> Queue.enqueue 10
                        |> Queue.enqueue 8
                        |> Queue.enqueue 6
                        |> Queue.enqueue 4
                        |> Queue.enqueue 2
                        |> Queue.all isEven
                        |> Expect.equal True
            ]

        --
        , describe "fromList + enqueue"
            [ test "6 5 4 3 2 1" <|
                \_ ->
                    Queue.fromList [ 4, 5, 6 ]
                        |> Queue.enqueue 3
                        |> Queue.enqueue 2
                        |> Queue.enqueue 1
                        |> Queue.all isEven
                        |> Expect.equal False

            --
            , test "10 8 6 4 2" <|
                \_ ->
                    Queue.fromList [ 10, 8, 6 ]
                        |> Queue.enqueue 4
                        |> Queue.enqueue 2
                        |> Queue.all isEven
                        |> Expect.equal True
            ]

        --
        , fuzz (Fuzz.list Fuzz.int) "reverse" <|
            \list ->
                Queue.fromList list
                    |> Queue.reverse
                    |> Queue.all isEven
                    |> Expect.equal (List.all isEven (List.reverse list))
        ]


memberSuit : Test
memberSuit =
    describe "Queue.member"
        [ test "[]" <|
            \_ ->
                Queue.fromList []
                    |> Queue.member 9
                    |> Expect.equal False

        --
        , test "[ 1, 2, 3, 4 ]" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4 ]
                    |> Queue.member 9
                    |> Expect.equal False

        --
        , test "[ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                    |> Queue.member 9
                    |> Expect.equal True
        ]


maximumSuit : Test
maximumSuit =
    fuzz (Fuzz.list Fuzz.int) "Queue.maximum" <|
        \list ->
            Queue.fromList list
                |> Queue.maximum
                |> Expect.equal (List.maximum list)


minimumSuit : Test
minimumSuit =
    fuzz (Fuzz.list Fuzz.int) "Queue.minimum" <|
        \list ->
            Queue.fromList list
                |> Queue.minimum
                |> Expect.equal (List.minimum list)



-- T R A N S F O R M


mapSuit : Test
mapSuit =
    describe "Queue.map"
        [ fuzz (Fuzz.list Fuzz.float) "transforms" <|
            \list ->
                Queue.fromList list
                    |> Queue.map round
                    |> Queue.toList
                    |> Expect.equalLists (List.map round list)

        --
        , test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.map ((*) 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 2, 4, 6, 8, 10, 12 ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 6
                    |> Queue.enqueue 5
                    |> Queue.enqueue 4
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.map ((*) 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 2, 4, 6, 8, 10, 12 ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.map ((*) 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 2, 4, 6, 8, 10, 12 ]

        --
        , test "reverse" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.reverse
                    |> Queue.map ((*) 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 12, 10, 8, 6, 4, 2 ]
        ]


indexedMapSuit : Test
indexedMapSuit =
    describe "Queue.indexedMap"
        [ test "transforms" <|
            \_ ->
                Queue.range 1 5
                    |> Queue.indexedMap (\ind int -> String.fromInt ind ++ String.fromInt int)
                    |> Queue.toList
                    |> Expect.equalLists [ "41", "32", "23", "14", "05" ]

        --
        , test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.indexedMap Tuple.pair
                    |> Queue.toList
                    |> Expect.equalLists [ ( 5, 1 ), ( 4, 2 ), ( 3, 3 ), ( 2, 4 ), ( 1, 5 ), ( 0, 6 ) ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 6
                    |> Queue.enqueue 5
                    |> Queue.enqueue 4
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.indexedMap Tuple.pair
                    |> Queue.toList
                    |> Expect.equalLists [ ( 5, 1 ), ( 4, 2 ), ( 3, 3 ), ( 2, 4 ), ( 1, 5 ), ( 0, 6 ) ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.indexedMap Tuple.pair
                    |> Queue.toList
                    |> Expect.equalLists [ ( 5, 1 ), ( 4, 2 ), ( 3, 3 ), ( 2, 4 ), ( 1, 5 ), ( 0, 6 ) ]

        --
        , test "reverse" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.reverse
                    |> Queue.indexedMap Tuple.pair
                    |> Queue.toList
                    |> Expect.equalLists [ ( 5, 6 ), ( 4, 5 ), ( 3, 4 ), ( 2, 3 ), ( 1, 2 ), ( 0, 1 ) ]
        ]


foldlSuite : Test
foldlSuite =
    describe "Queue.foldl"
        [ test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.foldl ((::) << String.fromInt) []
                    |> Expect.equalLists [ "1", "2", "3", "4", "5", "6" ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 6
                    |> Queue.enqueue 5
                    |> Queue.enqueue 4
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.foldl ((::) << String.fromInt) []
                    |> Expect.equalLists [ "1", "2", "3", "4", "5", "6" ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.foldl ((::) << String.fromInt) []
                    |> Expect.equalLists [ "1", "2", "3", "4", "5", "6" ]
        ]


foldrSuite : Test
foldrSuite =
    describe "Queue.foldr"
        [ test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.foldr ((::) << String.fromInt) []
                    |> Expect.equalLists [ "6", "5", "4", "3", "2", "1" ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 6
                    |> Queue.enqueue 5
                    |> Queue.enqueue 4
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.foldr ((::) << String.fromInt) []
                    |> Expect.equalLists [ "6", "5", "4", "3", "2", "1" ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.foldr ((::) << String.fromInt) []
                    |> Expect.equalLists [ "6", "5", "4", "3", "2", "1" ]
        ]


filterSuite : Test
filterSuite =
    describe "Queue.filter"
        [ test "keep all" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filter (always True)
                    |> Queue.toList
                    |> Expect.equalLists [ 1, 2, 3, 4, 5, 6 ]

        --
        , test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filter ((==) 0 << modBy 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 2, 4, 6 ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 6
                    |> Queue.enqueue 5
                    |> Queue.enqueue 4
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.filter ((==) 0 << modBy 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 2, 4, 6 ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.filter ((==) 0 << modBy 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 2, 4, 6 ]
        ]


filterMapSuite : Test
filterMapSuite =
    describe "Queue.filterMap"
        [ test "keep all" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filterMap Just
                    |> Queue.toList
                    |> Expect.equalLists [ 1, 2, 3, 4, 5, 6 ]

        --
        , test "map all" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filterMap (Just << String.fromInt)
                    |> Queue.toList
                    |> Expect.equalLists [ "1", "2", "3", "4", "5", "6" ]

        --
        , test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.filterMap
                        (\el ->
                            if el > 3 then
                                Nothing

                            else
                                Just el
                        )
                    |> Queue.toList
                    |> Expect.equalLists [ 1, 2, 3 ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 6
                    |> Queue.enqueue 5
                    |> Queue.enqueue 4
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.filterMap
                        (\el ->
                            if el > 3 then
                                Nothing

                            else
                                Just el
                        )
                    |> Queue.toList
                    |> Expect.equalLists [ 1, 2, 3 ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.filterMap
                        (\el ->
                            if el > 3 then
                                Nothing

                            else
                                Just el
                        )
                    |> Queue.toList
                    |> Expect.equalLists [ 1, 2, 3 ]
        ]


reverseSuite : Test
reverseSuite =
    describe "Queue.reverse"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equalLists []

        --
        , fuzz Fuzz.string "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equalLists [ val ]

        --
        , fuzz (Fuzz.list Fuzz.int) "fromList" <|
            \list ->
                Queue.fromList list
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equalLists (List.reverse list)

        --
        , test "range" <|
            \_ ->
                Queue.range 0 5
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equalLists [ 5, 4, 3, 2, 1, 0 ]

        --
        , fuzz2 (Fuzz.intRange 0 4) (Fuzz.intRange 5 10) "enqueue" <|
            \first last ->
                Queue.fromList [ -4, -3, -2, -1, first ]
                    |> Queue.enqueue last
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equalLists [ first, -1, -2, -3, -4, last ]

        --
        , fuzz (Fuzz.intRange 0 5) "dequeue" <|
            \second ->
                Queue.fromList [ -4, -3, -2, second, -1 ]
                    |> Queue.dequeue
                    |> Tuple.second
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equalLists [ second, -2, -3, -4 ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.reverse
                    |> Queue.toList
                    |> Expect.equal [ 6, 5, 4, 3, 2, 1 ]
        ]
