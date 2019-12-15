module QueueTests exposing (..)

import Expect
import Fuzz
import Queue
import Test exposing (Test, concat, describe, fuzz, fuzz2, test)


createSuite : Test
createSuite =
    concat
        [ test "Queue.empty" <|
            \_ ->
                Queue.toList Queue.empty
                    |> Expect.equalLists []

        --
        , fuzz Fuzz.string "Queue.singleton" <|
            \val ->
                Queue.singleton val
                    |> Queue.toList
                    |> Expect.equalLists [ val ]

        --
        , fuzz (Fuzz.list Fuzz.string) "Queue.fromList" <|
            \values ->
                Queue.fromList values
                    |> Queue.toList
                    |> Expect.equalLists values

        --
        , describe "Queue.repeat"
            [ fuzz Fuzz.string "zero times" <|
                \val ->
                    Queue.repeat 0 val
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat 0 val)
            , fuzz2 (Fuzz.intRange 1 100) Fuzz.string "multiple times" <|
                \times val ->
                    Queue.repeat times val
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat times val)
            ]

        --
        , describe "Queue.range"
            [ fuzz2 (Fuzz.intRange 1 10) (Fuzz.intRange -10 0) "lo > hi" <|
                \lo hi ->
                    Queue.range lo hi
                        |> Queue.toList
                        |> Expect.equalLists (List.range lo hi)
            , fuzz Fuzz.int "lo == hi" <|
                \lo ->
                    Queue.range lo lo
                        |> Queue.toList
                        |> Expect.equalLists (List.range lo lo)
            , fuzz2 (Fuzz.intRange -10 1) (Fuzz.intRange 0 10) "lo < hi" <|
                \lo hi ->
                    Queue.range lo hi
                        |> Queue.toList
                        |> Expect.equalLists (List.range lo hi)
            ]
        ]
