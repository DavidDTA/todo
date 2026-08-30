module Errors.ConcurrentTask exposing (map2)

import ConcurrentTask
import Errors


map2 fn a b =
    ConcurrentTask.map2
        (\resultA resultB ->
            case ( resultA, resultB ) of
                ( Err errA, Err errB ) ->
                    ConcurrentTask.fail (Errors.construct errA [ errB ])

                ( Err err, _ ) ->
                    ConcurrentTask.fail (Errors.singleton err)

                ( _, Err err ) ->
                    ConcurrentTask.fail (Errors.singleton err)

                ( Ok okA, Ok okB ) ->
                    fn okA okB
        )
        (ConcurrentTask.map Ok a
            |> ConcurrentTask.onError (Err >> ConcurrentTask.succeed)
        )
        (ConcurrentTask.map Ok b
            |> ConcurrentTask.onError (Err >> ConcurrentTask.succeed)
        )
