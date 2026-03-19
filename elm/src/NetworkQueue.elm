module NetworkQueue exposing
    ( NetworkQueue
    , Safety(..)
    , Token
    , dequeue
    , empty
    , enqueue
    , fold
    , halt
    )

import AutoDict
import Deque


type NetworkQueue request reason
    = NetworkQueue
        { current :
            AutoDict.AutoDict { request : request, safety : Safety, haltReason : Maybe reason }
        , future : Deque.Deque { request : request, safety : Safety }
        }


type Safety
    = Safe
    | Idempotent
    | Unsafe


type Token
    = Token AutoDict.Key


empty =
    NetworkQueue { current = AutoDict.empty, future = Deque.empty }


enqueue requestToCommand safety request (NetworkQueue { current, future }) =
    { current = current
    , future = Deque.pushBack { request = request, safety = safety } future
    , cmd = Cmd.none
    }
        |> advanceQueue requestToCommand
        |> wrap


dequeue requestToCommand (Token key) (NetworkQueue { current, future }) =
    { current = AutoDict.remove key current
    , future = future
    , cmd = Cmd.none
    }
        |> advanceQueue requestToCommand
        |> wrap


halt requestToCommand (Token key) reason (NetworkQueue { current, future }) =
    { current =
        AutoDict.update key
            (Maybe.andThen
                (\entry ->
                    Just { entry | haltReason = Just reason }
                )
            )
            current
    , future = future
    , cmd = Cmd.none
    }
        |> advanceQueue requestToCommand
        |> wrap


type State reason
    = InProgress
    | Halted reason
    | Queued


fold fn init (NetworkQueue { current, future }) =
    Deque.foldl
        (\_ acc -> fn Queued acc)
        (AutoDict.foldl
            (\_ { haltReason } acc ->
                fn
                    (haltReason
                        |> Maybe.map Halted
                        |> Maybe.withDefault InProgress
                    )
                    acc
            )
            init
            current
        )
        future


advanceQueue requestToCommand { current, future, cmd } =
    case Deque.first future of
        Nothing ->
            { current = current, future = future, cmd = cmd }

        Just front ->
            if AutoDict.isEmpty current || front.safety == Safe && AutoDict.foldl (\k v acc -> acc && v.safety == Safe) True current then
                let
                    { dict, key } =
                        AutoDict.insert
                            { request = front.request
                            , safety = front.safety
                            , haltReason = Nothing
                            }
                            current
                in
                advanceQueue
                    requestToCommand
                    { current = dict
                    , future = Deque.dropLeft 1 future
                    , cmd = Cmd.batch [ cmd, requestToCommand (Token key) front.request ]
                    }

            else
                { current = current, future = future, cmd = cmd }


wrap { current, future, cmd } =
    { queue = NetworkQueue { current = current, future = future }
    , cmd = cmd
    }
