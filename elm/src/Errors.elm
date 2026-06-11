module Errors exposing (Errors, append, construct, map, singleton, toList)


type Errors error
    = Errors { first : error, rest : List error }


singleton error =
    Errors { first = error, rest = [] }


construct head tail =
    Errors { first = head, rest = tail }


toList (Errors { first, rest }) =
    first :: rest


append tail (Errors head) =
    Errors { first = head.first, rest = head.rest ++ [ tail ] }


concat tail (Errors head) =
    Errors { first = head.first, rest = head.rest ++ toList tail }


map fn (Errors { first, rest }) =
    Errors { first = fn first, rest = List.map fn rest }
