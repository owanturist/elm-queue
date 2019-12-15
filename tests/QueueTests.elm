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
            \list ->
                Queue.fromList list
                    |> Queue.toList
                    |> Expect.equalLists list

        --
        , describe "Queue.repeat"
            [ fuzz Fuzz.string "zero n" <|
                \val ->
                    Queue.repeat 0 val
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat 0 val)
            , fuzz2 (Fuzz.intRange 1 100) Fuzz.string "multiple n" <|
                \n val ->
                    Queue.repeat n val
                        |> Queue.toList
                        |> Expect.equalLists (List.repeat n val)
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
            , fuzz2 (Fuzz.intRange -10 -1) (Fuzz.intRange 0 10) "lo < hi" <|
                \lo hi ->
                    Queue.range lo hi
                        |> Queue.toList
                        |> Expect.equalLists (List.range lo hi)
            ]
        ]


querySuit : Test
querySuit =
    concat
        [ describe "Queue.peek"
            [ test "empty" <|
                \_ ->
                    Queue.empty
                        |> Queue.peek
                        |> Expect.equal Nothing
            , fuzz Fuzz.string "singleton" <|
                \val ->
                    Queue.singleton val
                        |> Queue.peek
                        |> Expect.equal (Just val)
            , fuzz2 (Fuzz.intRange 1 100) Fuzz.string "repeat" <|
                \n val ->
                    Queue.repeat n val
                        |> Queue.peek
                        |> Expect.equal (Just val)
            , fuzz2 (Fuzz.intRange -10 -1) (Fuzz.intRange 0 10) "range" <|
                \lo hi ->
                    Queue.range lo hi
                        |> Queue.peek
                        |> Expect.equal (Just hi)
            ]
        ]
