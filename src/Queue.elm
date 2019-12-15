module Queue exposing (Queue, empty, fromList, range, repeat, singleton, toList)


type Queue a
    = Empty
    | Queue (List a) (List a) a



-- C R E A T E


empty : Queue a
empty =
    Empty


singleton : a -> Queue a
singleton element =
    Queue [] [] element


fromList : List a -> Queue a
fromList list =
    case List.reverse list of
        [] ->
            Empty

        head :: output ->
            Queue [] output head


repeat : Int -> a -> Queue a
repeat times element =
    if times < 1 then
        Empty

    else
        Queue [] (List.repeat (times - 1) element) element


range : Int -> Int -> Queue Int
range lo hi =
    if lo > hi then
        Empty

    else
        Queue [] (rangeHelp lo hi []) hi


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi acc =
    if lo < hi then
        rangeHelp (lo + 1) hi (lo :: acc)

    else
        acc


enqueue : a -> Queue a -> Queue a
enqueue element queue =
    case queue of
        Empty ->
            Queue [] [] element

        Queue input output head ->
            Queue (element :: input) output head


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue queue =
    case queue of
        Empty ->
            ( Nothing, Empty )

        Queue input [] head ->
            ( Just head
            , case List.reverse input of
                [] ->
                    Empty

                nextHead :: nextOutStack ->
                    Queue [] nextOutStack nextHead
            )

        Queue input (nextHead :: nextOutStack) head ->
            ( Just head
            , Queue input nextOutStack nextHead
            )



-- T R A N S F O R M


map : (a -> b) -> Queue a -> Queue b
map fn queue =
    case queue of
        Empty ->
            Empty

        Queue input output head ->
            Queue []
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

        Queue input output head ->
            Queue []
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

        Queue input output head ->
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

        Queue input output head ->
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

        Queue input output _ ->
            List.length input + List.length output + 1


member : a -> Queue a -> Bool
member element queue =
    case queue of
        Empty ->
            False

        Queue input output head ->
            head == element || List.member element input || List.member element output


all : (a -> Bool) -> Queue a -> Bool
all check queue =
    case queue of
        Empty ->
            True

        Queue input output head ->
            check head || List.all check input || List.all check output


any : (a -> Bool) -> Queue a -> Bool
any check queue =
    case queue of
        Empty ->
            False

        Queue input output head ->
            check head || List.any check input || List.any check output


toList : Queue a -> List a
toList queue =
    case queue of
        Empty ->
            []

        Queue input output head ->
            List.foldl (::) input (head :: output)
