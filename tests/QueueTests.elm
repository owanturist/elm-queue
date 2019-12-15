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
        ]
