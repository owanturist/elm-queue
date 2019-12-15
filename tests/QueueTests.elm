module QueueTests exposing (..)

import Expect
import Fuzz
import Queue
import Test exposing (Test, describe, fuzz, fuzz2, test)



-- C O N S T R U C T I O N


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
        ]



-- T R A N S F O R M


mapSuit : Test
mapSuit =
    describe "Queue.map"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.map String.length
                    |> Queue.toList
                    |> Expect.equalLists []

        --
        , fuzz Fuzz.string "singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.map String.length
                    |> Queue.toList
                    |> Expect.equalLists [ String.length val ]

        --
        , fuzz (Fuzz.list Fuzz.string) "fromList" <|
            \list ->
                Queue.fromList list
                    |> Queue.map String.length
                    |> Queue.toList
                    |> Expect.equalLists (List.map String.length list)

        --
        , test "range" <|
            \_ ->
                Queue.range 0 5
                    |> Queue.map ((*) 2)
                    |> Queue.toList
                    |> Expect.equalLists [ 0, 2, 4, 6, 8, 10 ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 0
                    |> Queue.enqueue 1
                    |> Queue.enqueue 2
                    |> Queue.enqueue 3
                    |> Queue.enqueue 4
                    |> Queue.map ((+) 1)
                    |> Queue.toList
                    |> Expect.equalLists [ 5, 4, 3, 2, 1 ]
        ]


indexedMapSuit : Test
indexedMapSuit =
    describe "Queue.indexedMap"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.indexedMap (\i e -> ( i, String.length e ))
                    |> Queue.toList
                    |> Expect.equalLists []

        --
        , test "singleton" <|
            \_ ->
                Queue.singleton "str"
                    |> Queue.indexedMap (\i e -> ( i, String.length e ))
                    |> Queue.toList
                    |> Expect.equalLists [ ( 0, 3 ) ]

        --
        , test "fromList" <|
            \_ ->
                Queue.fromList [ "a", "aa", "aaa" ]
                    |> Queue.indexedMap (\i e -> ( i, String.length e ))
                    |> Queue.toList
                    |> Expect.equalLists [ ( 2, 1 ), ( 1, 2 ), ( 0, 3 ) ]

        --
        , test "range" <|
            \_ ->
                Queue.range 0 5
                    |> Queue.indexedMap (\i e -> ( i, e * 2 ))
                    |> Queue.toList
                    |> Expect.equalLists [ ( 5, 0 ), ( 4, 2 ), ( 3, 4 ), ( 2, 6 ), ( 1, 8 ), ( 0, 10 ) ]

        --
        , test "enqueue" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 0
                    |> Queue.enqueue 1
                    |> Queue.enqueue 2
                    |> Queue.enqueue 3
                    |> Queue.enqueue 4
                    |> Queue.indexedMap (\i e -> ( i, e + 1 ))
                    |> Queue.toList
                    |> Expect.equalLists [ ( 4, 5 ), ( 3, 4 ), ( 2, 3 ), ( 1, 2 ), ( 0, 1 ) ]
        ]


foldlSuite : Test
foldlSuite =
    describe "Queue.foldl"
        [ test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.foldl (::) []
                    |> Expect.equalLists [ 1, 2, 3, 4, 5, 6 ]

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
                    |> Queue.foldl (::) []
                    |> Expect.equalLists [ 1, 2, 3, 4, 5, 6 ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.foldl (::) []
                    |> Expect.equalLists [ 1, 2, 3, 4, 5, 6 ]
        ]


foldrSuite : Test
foldrSuite =
    describe "Queue.foldr"
        [ test "fromList" <|
            \_ ->
                Queue.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> Queue.foldr (::) []
                    |> Expect.equalLists [ 6, 5, 4, 3, 2, 1 ]

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
                    |> Queue.foldr (::) []
                    |> Expect.equalLists [ 6, 5, 4, 3, 2, 1 ]

        --
        , test "fromList + enqueue" <|
            \_ ->
                Queue.fromList [ 4, 5, 6 ]
                    |> Queue.enqueue 3
                    |> Queue.enqueue 2
                    |> Queue.enqueue 1
                    |> Queue.foldr (::) []
                    |> Expect.equalLists [ 6, 5, 4, 3, 2, 1 ]
        ]
