module Loadable.Element exposing
    ( blank
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (..)
import Loadable.Types exposing (..)
import Task exposing (Task)


init : (flags -> Task e ( innerModel, Cmd msg )) -> flags -> ( Model flags innerModel e, Cmd (Msg innerModel msg e) )
init load flags =
    ( Loading flags, load flags |> Task.attempt resultToMsg )


blank : Html msg
blank =
    text ""


view :
    Maybe (flags -> Html (Msg innerModel msg e))
    -> Maybe (e -> Html (Msg innerModel msg e))
    -> (innerModel -> Html msg)
    -> Model flags innerModel e
    -> Html (Msg innerModel msg e)
view loadingView errorView innerView outerModel =
    case outerModel of
        Loading flags ->
            Maybe.withDefault (always blank) loadingView flags

        Error e ->
            Maybe.withDefault (always blank) errorView e

        Loaded model ->
            Html.map ToInner (innerView model)


subscriptions : (innerModel -> Sub msg) -> Model flags innerModel e -> Sub (Msg innerModel msg e)
subscriptions innerSubscriptions outerModel =
    case outerModel of
        Loaded model ->
            Sub.map ToInner (innerSubscriptions model)

        _ ->
            Sub.none


update :
    (msg -> innerModel -> ( innerModel, Cmd msg ))
    -> Maybe (e -> Cmd (Msg innerModel msg e))
    -> Msg innerModel msg e
    -> Model flags innerModel e
    -> ( Model flags innerModel e, Cmd (Msg innerModel msg e) )
update innerUpdate failCmd msg outerModel =
    case ( outerModel, msg ) of
        -- When there's a load error, fail.
        ( _, LoadError e ) ->
            let
                f =
                    Maybe.withDefault (always Cmd.none) failCmd
            in
            ( Error e, f e )

        -- When successfully loaded, then get the initial state by passing
        -- the initial load and flags to the init function.
        ( Loading flags, LoadSuccess load innerInitialMsg ) ->
            ( Loaded load, Cmd.map ToInner innerInitialMsg )

        -- When it is loaded, and there's an inner message, pass it to the underlying fn.
        ( Loaded model, ToInner msg_ ) ->
            let
                ( newModel, cmd ) =
                    innerUpdate msg_ model
            in
            ( Loaded newModel, Cmd.map ToInner cmd )

        -- Otherwise just return the state with no command.
        _ ->
            ( outerModel, Cmd.none )
