module Loadable exposing (element, Model, Msg)

{-|

@docs element, Model, Msg

-}

import Browser exposing (..)
import Browser.Navigation exposing (..)
import Html exposing (Html, text)
import Task exposing (Task)


{-| Behaves a lot like element from the Html package, with a few additions.
-}
element :
    { update : innerMsg -> innerModel -> ( innerModel, Cmd innerMsg )
    , subscriptions : innerModel -> Sub innerMsg
    , view : innerModel -> Html innerMsg

    -- Takes the flags it's given and returns the initial state of the model.
    , load : flags -> Task e ( innerModel, Cmd innerMsg )

    -- Takes any error that was produced during the load step and returns a command to be run.
    -- (This can be used to send a message out through a port for error reporting.)
    , failCmd : Maybe (e -> Cmd (Msg innerModel innerMsg e))

    -- Takes your flags and displays HTML while the page loads.
    , loadingView : Maybe (flags -> Html (Msg innerModel innerMsg e))

    -- Takes any error that was produced during the load step and displays HTML.
    , errorView : Maybe (e -> Html (Msg innerModel innerMsg e))
    }
    -> Program flags (Model flags innerModel e) (Msg innerModel innerMsg e)
element { update, view, subscriptions, load, failCmd, loadingView, errorView } =
    Browser.element
        { init = lInit load
        , update = lUpdate update failCmd
        , subscriptions = lSubscriptions subscriptions
        , view = lView loadingView errorView view
        }


{-| The Model that wraps your Model.
-}
type Model flags innerModel error
    = Loading flags
    | Loaded innerModel
    | Error error


{-| The Msg that wraps your Msg. LoadSuccess/LoadError triggers depending on the result of your load function. ToInner is the msg that
-- passes all your program messages on to your update function.
-}
type Msg innerModel msg error
    = LoadSuccess innerModel (Cmd msg)
    | LoadError error
    | ToInner msg


lInit : (flags -> Task e ( innerModel, Cmd msg )) -> flags -> ( Model flags innerModel e, Cmd (Msg innerModel msg e) )
lInit load flags =
    ( Loading flags, load flags |> Task.attempt resultToMsg )


resultToMsg : Result error ( innerModel, Cmd innerInitialMsg ) -> Msg innerModel innerInitialMsg error
resultToMsg result =
    case result of
        Ok ( innerModel, innerInitialMsg ) ->
            LoadSuccess innerModel innerInitialMsg

        Err e ->
            LoadError e


blank : Html msg
blank =
    text ""


lView :
    Maybe (flags -> Html (Msg innerModel msg e))
    -> Maybe (e -> Html (Msg innerModel msg e))
    -> (innerModel -> Html msg)
    -> Model flags innerModel e
    -> Html (Msg innerModel msg e)
lView loadingView errorView view outerModel =
    case outerModel of
        Loading flags ->
            Maybe.withDefault (always blank) loadingView flags

        Error e ->
            Maybe.withDefault (always blank) errorView e

        Loaded model ->
            Html.map ToInner (view model)


lSubscriptions : (innerModel -> Sub msg) -> Model flags innerModel e -> Sub (Msg innerModel msg e)
lSubscriptions subscriptions outerModel =
    case outerModel of
        Loaded model ->
            Sub.map ToInner (subscriptions model)

        _ ->
            Sub.none


lUpdate :
    (msg -> innerModel -> ( innerModel, Cmd msg ))
    -> Maybe (e -> Cmd (Msg innerModel msg e))
    -> Msg innerModel msg e
    -> Model flags innerModel e
    -> ( Model flags innerModel e, Cmd (Msg innerModel msg e) )
lUpdate update failCmd msg outerModel =
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
                    update msg_ model
            in
            ( Loaded newModel, Cmd.map ToInner cmd )

        -- Otherwise just return the state with no command.
        _ ->
            ( outerModel, Cmd.none )
