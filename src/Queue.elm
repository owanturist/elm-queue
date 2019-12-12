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
