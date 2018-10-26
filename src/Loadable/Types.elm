module Loadable.Types exposing (Model(..), Msg(..), resultToMsg)


type Model flags innerModel error
    = Loading flags
    | Loaded innerModel
    | Error error


type Msg innerModel msg error
    = LoadSuccess innerModel (Cmd msg)
    | LoadError error
    | ToInner msg


resultToMsg : Result error ( innerModel, Cmd innerInitialMsg ) -> Msg innerModel innerInitialMsg error
resultToMsg result =
    case result of
        Ok ( innerModel, innerInitialMsg ) ->
            LoadSuccess innerModel innerInitialMsg

        Err e ->
            LoadError e
