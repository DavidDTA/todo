module Endpoint exposing
    ( Endpoint
    , Handlers
    , PathComponent
    , addHandler
    , endpoint
    , fixed
    , getHandler
    , handlers
    , mapHandlers
    , request
    , wildcard
    )

import Dict
import Http
import Json.Decode
import List
import String
import Url


type Available
    = Available


type PathComponent
    = Fixed String
    | Wildcard


type Endpoint request response
    = Endpoint
        { method : String
        , path : List PathComponent
        , request : request
        , response : response
        }


fixed =
    Fixed


wildcard =
    Wildcard


endpoint : String -> List PathComponent -> { request : String -> List String -> req, response : res } -> Endpoint req res
endpoint method path r =
    Endpoint
        { method = method
        , path = path
        , request =
            r.request method
                (List.map
                    (\pathSegment ->
                        case pathSegment of
                            Fixed string ->
                                string

                            Wildcard ->
                                "*"
                    )
                    path
                )
        , response = r.response
        }


request (Endpoint e) =
    e.request


type Handlers response
    = Handlers
        { fallback : response
        , registered : Result () (HandlersTree response)
        }


type HandlersTree a
    = UnregisteredNode
    | FixedNodes (Dict.Dict String { value : Maybe a, next : HandlersTree a })
    | WildcardNode { value : Maybe a, next : HandlersTree a }


handlers fallback =
    Handlers { fallback = fallback, registered = Ok UnregisteredNode }


addHandler : Endpoint req (impl -> handler) -> impl -> Handlers handler -> Handlers handler
addHandler (Endpoint { method, path, response }) handler (Handlers handlers_) =
    Handlers
        { handlers_
            | registered =
                handlers_.registered
                    |> Result.andThen (addHandlerInner (Fixed method :: path) (response handler))
        }


addHandlerInner : List PathComponent -> a -> HandlersTree a -> Result () (HandlersTree a)
addHandlerInner path value handlers_ =
    case ( path, handlers_ ) of
        ( Wildcard :: [], UnregisteredNode ) ->
            Ok (WildcardNode { value = Just value, next = UnregisteredNode })

        ( Wildcard :: remainingPath, UnregisteredNode ) ->
            addHandlerInner remainingPath value UnregisteredNode
                |> Result.andThen
                    (\next ->
                        Ok (WildcardNode { value = Nothing, next = next })
                    )

        ( [ Fixed pathSegment ], UnregisteredNode ) ->
            Ok (FixedNodes (Dict.singleton pathSegment { value = Just value, next = UnregisteredNode }))

        ( (Fixed pathSegment) :: remainingPath, UnregisteredNode ) ->
            addHandlerInner remainingPath value UnregisteredNode
                |> Result.andThen
                    (\next ->
                        Ok (FixedNodes (Dict.singleton pathSegment { value = Nothing, next = next }))
                    )

        ( Wildcard :: [], WildcardNode node ) ->
            case node.value of
                Nothing ->
                    Ok (WildcardNode { value = Just value, next = node.next })

                Just _ ->
                    Err ()

        ( Wildcard :: remainingPath, WildcardNode node ) ->
            addHandlerInner remainingPath value node.next
                |> Result.andThen
                    (\next ->
                        Ok (WildcardNode { value = node.value, next = next })
                    )

        ( [ Fixed pathSegment ], FixedNodes nodes ) ->
            let
                node =
                    Dict.get pathSegment nodes
                        |> Maybe.withDefault { value = Nothing, next = UnregisteredNode }
            in
            case node.value of
                Just _ ->
                    Err ()

                Nothing ->
                    Ok (FixedNodes (Dict.insert pathSegment { value = Just value, next = node.next } nodes))

        ( (Fixed pathSegment) :: remainingPath, FixedNodes nodes ) ->
            let
                node =
                    Dict.get pathSegment nodes
                        |> Maybe.withDefault { value = Nothing, next = UnregisteredNode }
            in
            addHandlerInner remainingPath value node.next
                |> Result.andThen
                    (\next ->
                        Ok (FixedNodes (Dict.insert pathSegment { value = node.value, next = next } nodes))
                    )

        _ ->
            Err ()


getHandler : String -> List String -> Handlers a -> a
getHandler method path (Handlers { fallback, registered }) =
    case registered of
        Err () ->
            fallback

        Ok tree ->
            getHandlerInner fallback (method :: path) tree


getHandlerInner fallback path tree =
    case ( path, tree ) of
        ( _ :: [], WildcardNode { value } ) ->
            Maybe.withDefault fallback value

        ( _ :: remainingPath, WildcardNode { next } ) ->
            getHandlerInner fallback remainingPath next

        ( segment :: [], FixedNodes nodes ) ->
            Dict.get segment nodes
                |> Maybe.andThen .value
                |> Maybe.withDefault fallback

        ( segment :: remainingPath, FixedNodes nodes ) ->
            Dict.get segment nodes
                |> Maybe.map (.next >> getHandlerInner fallback remainingPath)
                |> Maybe.withDefault fallback

        _ ->
            fallback


mapHandlers fn (Handlers { fallback, registered }) =
    Handlers
        { fallback = fn fallback
        , registered = Result.map (mapHandlersInner fn) registered
        }


mapHandlersInner fn tree =
    case tree of
        WildcardNode { value, next } ->
            WildcardNode { value = Maybe.map fn value, next = mapHandlersInner fn next }

        FixedNodes nodes ->
            FixedNodes (Dict.map (\_ { value, next } -> { value = Maybe.map fn value, next = mapHandlersInner fn next }) nodes)

        UnregisteredNode ->
            UnregisteredNode
