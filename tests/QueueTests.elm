module QueueTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Queue
import Test exposing (Test, describe, test)


createSuite : Test
createSuite =
    describe "Create"
        [ test "Queue.empty" <|
            \_ ->
                Queue.toList Queue.empty
                    |> Expect.equalLists []

        --
        , test "Queue.singleton" <|
            \_ ->
                Queue.singleton 0
                    |> Queue.toList
                    |> Expect.equalLists [ 0 ]

        --
        , describe "Queue.fromList"
            [ test "empty list" <|
                \_ ->
                    Queue.fromList []
                        |> Queue.toList
                        |> Expect.equalLists []
            , test "singleton list" <|
                \_ ->
                    Queue.fromList [ 0 ]
                        |> Queue.toList
                        |> Expect.equalLists [ 0 ]
            , test "batch list" <|
                \_ ->
                    Queue.fromList [ 0, 1, 2, 3, 4, 5 ]
                        |> Queue.toList
                        |> Expect.equalLists [ 0, 1, 2, 3, 4, 5 ]
            ]

        --
        , describe "Queue.repeat"
            [ test "zero times" <|
                \_ ->
                    Queue.repeat 0 1
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat 0 1)
            , test "one time" <|
                \_ ->
                    Queue.repeat 1 2
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat 1 2)
            , test "multiple times" <|
                \_ ->
                    Queue.repeat 9 0
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat 9 0)
            ]

        --
        , describe "Queue.range"
            [ test "lo > hi" <|
                \_ ->
                    Queue.range 1 0
                        |> Queue.toList
                        |> Expect.equalLists (List.range 1 0)
            , test "lo == hi" <|
                \_ ->
                    Queue.range 0 0
                        |> Queue.toList
                        |> Expect.equalLists (List.range 0 0)
            , test "lo < hi" <|
                \_ ->
                    Queue.range -5 5
                        |> Queue.toList
                        |> Expect.equalLists (List.range -5 5)
            ]
        ]
