module Ffi exposing (applyFunction, applyFunctionCmd, getProperty)

import ConcurrentTask
import Json.Decode
import Json.Encode


getProperty : Json.Encode.Value -> Json.Encode.Value -> Json.Decode.Decoder result -> ConcurrentTask.ConcurrentTask x result
getProperty object name decoder =
    ConcurrentTask.define
        { function = "property:get"
        , expect = ConcurrentTask.expectJson decoder
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "object", object )
                , ( "name", name )
                ]
        }


applyFunction : Json.Encode.Value -> Json.Encode.Value -> List Json.Encode.Value -> Json.Decode.Decoder result -> ConcurrentTask.ConcurrentTask x result
applyFunction function thisArg argsArray decoder =
    ConcurrentTask.define
        { function = "function:apply"
        , expect = ConcurrentTask.expectJson decoder
        , errors = ConcurrentTask.expectNoErrors
        , args =
            Json.Encode.object
                [ ( "function", function )
                , ( "thisArg", thisArg )
                , ( "argsArray", Json.Encode.list identity argsArray )
                ]
        }


applyFunctionCmd function thisArg argsArray port_ =
    Json.Encode.object
        [ ( "attemptId", Json.Encode.string "<cmd>" )
        , ( "taskId", Json.Encode.string "<cmd>" )
        , ( "function", Json.Encode.string "function:apply" )
        , ( "args"
          , Json.Encode.object
                [ ( "function", function )
                , ( "thisArg", thisArg )
                , ( "argsArray", Json.Encode.list identity argsArray )
                ]
          )
        ]
        |> List.singleton
        |> Json.Encode.list identity
        |> port_
