module Loadable.Application exposing (init, view)

import Browser exposing (..)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Loadable.Element exposing (blank)
import Loadable.Types exposing (..)
import Task exposing (Task)
import Url exposing (..)


init :
    (flags -> Url -> Key -> Task e ( innerModel, Cmd msg ))
    -> flags
    -> Url
    -> Key
    -> ( Model flags innerModel e, Cmd (Msg innerModel msg e) )
init load flags url key =
    ( Loading flags, load flags url key |> Task.attempt resultToMsg )


view :
    Maybe (flags -> Document (Msg innerModel msg e))
    -> Maybe (e -> Document (Msg innerModel msg e))
    -> (innerModel -> Document msg)
    -> Model flags innerModel e
    -> Document (Msg innerModel msg e)
view loadingView errorView innerView outerModel =
    case outerModel of
        Loading flags ->
            Maybe.withDefault
                (\_ ->
                    { title = "Loading..."
                    , body = []
                    }
                )
                loadingView
                flags

        Error e ->
            Maybe.withDefault
                (\_ ->
                    { title = "Error"
                    , body = []
                    }
                )
                errorView
                e

        Loaded model ->
            let
                { title, body } =
                    innerView model
            in
            { title = title
            , body = List.map (Html.map ToInner) body
            }
