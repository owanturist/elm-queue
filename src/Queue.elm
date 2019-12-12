module Queue exposing (Queue)


type Queue a
    = Empty
    | Queue (List a) (List a) a


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

        head :: outStack ->
            Queue [] outStack head


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
        Queue [] (rangeHelp lo (hi - 1) []) hi


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi acc =
    if lo >= hi then
        acc

    else
        rangeHelp (lo + 1) hi (lo :: acc)


enqueue : a -> Queue a -> Queue a
enqueue element queue =
    case queue of
        Empty ->
            Queue [] [] element

        Queue inStack outStack head ->
            Queue (element :: inStack) outStack head


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue queue =
    case queue of
        Empty ->
            ( Nothing, Empty )

        Queue inStack [] head ->
            ( Just head
            , case List.reverse inStack of
                [] ->
                    Empty

                nextHead :: nextOutStack ->
                    Queue [] nextOutStack nextHead
            )

        Queue inStack (nextHead :: nextOutStack) head ->
            ( Just head
            , Queue inStack nextOutStack nextHead
            )



-- T R A N S F O R M


map : (a -> b) -> Queue a -> Queue b
map fn queue =
    case queue of
        Empty ->
            Empty

        Queue inStack outStack head ->
            Queue []
                (List.foldl ((::) << fn) (List.map fn outStack) inStack)
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

        Queue inStack outStack head ->
            Queue []
                (List.foldl
                    (indexedMapReducer fn)
                    (List.foldr (indexedMapReducer fn) ( 1, [] ) outStack)
                    inStack
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

        Queue inStack outStack head ->
            List.foldr
                (foldReducer fn)
                (List.foldl
                    (foldReducer fn)
                    (fn head acc)
                    outStack
                )
                inStack


foldr : (a -> b -> b) -> b -> Queue a -> b
foldr fn acc queue =
    case queue of
        Empty ->
            acc

        Queue inStack outStack head ->
            List.foldl
                (foldReducer fn)
                (List.foldr
                    (foldReducer fn)
                    (fn head acc)
                    outStack
                )
                inStack



-- U T I L I T I E S


length : Queue a -> Int
length queue =
    case queue of
        Empty ->
            0

        Queue inStack outStack _ ->
            List.length inStack + List.length outStack + 1


member : a -> Queue a -> Bool
member element queue =
    case queue of
        Empty ->
            False

        Queue inStack outStack head ->
            head == element || List.member element inStack || List.member element outStack


all : (a -> Bool) -> Queue a -> Bool
all check queue =
    case queue of
        Empty ->
            True

        Queue inStack outStack head ->
            check head || List.all check inStack || List.all check outStack


any : (a -> Bool) -> Queue a -> Bool
any check queue =
    case queue of
        Empty ->
            False

        Queue inStack outStack head ->
            check head || List.any check inStack || List.any check outStack
