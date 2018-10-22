# alex-tan/loadable

This package allows you to separate the initial loading of resources in an Elm program
from the rest of the logic of your application.

Example: Let's say you have an Elm application that depends on a JSON payload being loaded.
By separating the initial load process from the rest of your application, it allows you to simplify all subsequent code
in your application because you no longer have to deal with as many Maybe values, therefore reducing the complexity and number
of code paths.


```elm
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
            , loadingView = Just (\flags -> text "Loading...")
            , errorView = Just (\error -> text "Something went wrong!")
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
```