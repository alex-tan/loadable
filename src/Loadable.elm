module Loadable exposing (element, application, Program)

{-|

@docs element, application, Program

-}

import Browser exposing (..)
import Browser.Navigation exposing (..)
import Html exposing (Html, text)
import Loadable.Application as A
import Loadable.Element as E
import Loadable.Types exposing (..)
import Task exposing (Task)
import Url exposing (..)


{-| An alias for the Browser.Program that gets produced from this package which makes
it easier to add type annotations for your programs.
-}
type alias Program flags model msg error =
    Platform.Program flags (Model flags model error) (Msg model msg error)


{-| Behaves a lot like application from the Browser package, with a few additions.

  - `load` replaces `init` and returns a task that will return your initial model and command.
  - `failCmd` can be used to send any error that results from the `load` task through a port.
  - `loadingView` can take flags and return a loading view.
  - `errorView` can take any error that results from `load` and displays an error message.

-}
application :
    { update : innerMsg -> innerModel -> ( innerModel, Cmd innerMsg )
    , subscriptions : innerModel -> Sub innerMsg
    , view : innerModel -> Document innerMsg
    , load : flags -> Url -> Key -> Task e ( innerModel, Cmd innerMsg )
    , failCmd : Maybe (e -> Cmd (Msg innerModel innerMsg e))
    , loadingView : Maybe (flags -> Document (Msg innerModel innerMsg e))
    , errorView : Maybe (e -> Document (Msg innerModel innerMsg e))
    , onUrlRequest : UrlRequest -> innerMsg
    , onUrlChange : Url -> innerMsg
    }
    -> Program flags innerModel innerMsg e
application { update, view, subscriptions, load, failCmd, loadingView, errorView, onUrlRequest, onUrlChange } =
    Browser.application
        { init = A.init load
        , update = E.update update failCmd
        , subscriptions = E.subscriptions subscriptions
        , view = A.view loadingView errorView view
        , onUrlRequest = onUrlRequest >> ToInner
        , onUrlChange = onUrlChange >> ToInner
        }


{-| Behaves a lot like element from the Browser package, with a few additions.

  - `load` replaces `init` and returns a task that will return your initial model and command.
  - `failCmd` can be used to send any error that results from the `load` task through a port.
  - `loadingView` can take flags and return a loading view.
  - `errorView` can take any error that results from `load` and displays an error message.

-}
element :
    { update : innerMsg -> innerModel -> ( innerModel, Cmd innerMsg )
    , subscriptions : innerModel -> Sub innerMsg
    , view : innerModel -> Html innerMsg
    , load : flags -> Task e ( innerModel, Cmd innerMsg )
    , failCmd : Maybe (e -> Cmd (Msg innerModel innerMsg e))
    , loadingView : Maybe (flags -> Html (Msg innerModel innerMsg e))
    , errorView : Maybe (e -> Html (Msg innerModel innerMsg e))
    }
    -> Program flags innerModel innerMsg e
element { update, view, subscriptions, load, failCmd, loadingView, errorView } =
    Browser.element
        { init = E.init load
        , update = E.update update failCmd
        , subscriptions = E.subscriptions subscriptions
        , view = E.view loadingView errorView view
        }
