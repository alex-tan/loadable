module Loadable exposing (element, Model, Msg)

{-| This package allows you to separate the initial loading of resources in an Elm program
from the rest of the logic of your application.

Example: Let's say you have an Elm application that depends on a JSON payload being loaded.
By separating the initial load process from the rest of your application, it allows you to simplify all subsequent code
in your application because you no longer have to deal with as many Maybe values, therefore reducing the complexity and number
of code paths.

    import Animal
    import Help.Loadable as L
    import Html exposing (text)
    import Http
    import Task exposing (Task)


    -- Define your Flags as you normally would.
    type alias Flags =
        { frogID : Int
        , birdID : Int
        }

    -- Define your Model, but without any unnecessary Maybe types.
    type alias Model =
        { frog : Int
        , bird : Int
        }

    -- Define your Msg as you normally would.
    type Msg
        = Noop

    -- Instead of using Browser.element directly, you can use Loadable.element to get back
    -- a Program of Loadable Msg and Loadable Model which wraps your application.
    -- There are some additional options that you can find documented below.
    main : Program Flags (L.Model Flags Model Http.Error) (L.Msg Model Msg Http.Error)
    main =
        L.element
            { update = \msg model -> model
            , subscriptions = subscriptions
            , view = text ""
            , load = load
            , failCmd = Nothing
            , loadingView = Nothing
            , errorView = Nothing
            }

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Sub.none

    -- This is what sets a Loadable element apart from a normal Browser.element. You can join
    -- however many Tasks you want together to return your starting (Model, Msg) when the page loads.
    -- In the meantime, the `loadingView` you specify will display info to the user that the page is loading
    -- and if anything fails, your `errorView` can be used to display an error message to the user.
    load : Flags -> Task Http.Error ( Model, Cmd Msg )
    load flags =
        let
            { birdID, frogID } =
                flags

            birdTask =
                Animal.getByID birdID |> Http.toTask

            frogTask =
                Animal.getByID frogID |> Http.toTask
        in
        Task.map2
            (\bird frog ->
                ( { bird = bird, frog = frog }
                , Cmd.none
                )
            )
            birdTask
            frogTask


# Definition

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


{-| The Model that wraps your Model. Can either be in a loading state, loaded state, or an error state (if the loading step fails.)
-}
type Model flags innerModel e
    = Loading flags
    | Loaded innerModel
    | Error e


{-| The Msg that wraps your Msg. LoadSuccess/LoadError triggers depending on the result of your load function. ToInner is the msg that
-- passes all your program messages on to your update function.
-}
type Msg innerModel msg e
    = LoadSuccess innerModel (Cmd msg)
    | LoadError e
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
