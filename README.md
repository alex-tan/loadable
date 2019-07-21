# alex-tan/loadable

This package allows you to separate the initial loading of resources in an Elm program
from the rest of the logic of your application.

Example: Let's say you have an Elm application that depends on a JSON payload being loaded.
By separating the initial load process from the rest of your application, it allows you to simplify all subsequent code
in your application because you no longer have to deal with as many Maybe values, therefore reducing the complexity and number of code paths.

Credit to [@bamorim](https://github.com/bamorim) for the initial implementation.


```elm
    import Animal exposing (Animal)
    import Loadable as L
    import Html exposing (div, text)
    import Http
    import Task exposing (Task)
    import Ports


    -- Define your Flags as you normally would.
    type alias Flags =
        { frogID : Int
        , birdID : Int
        }

    -- Define your Model, but without any unnecessary Maybe
    -- types.
    type alias Model =
        { frog : Animal
        , bird : Animal
        }

    -- Define your Msg as you normally would.
    type Msg
        = Noop

    -- Instead of using Browser.element directly, you can use
    -- Loadable.element to get back an Loadable.Program which is a
    -- special alias for Browser.program. (There's also a Loadable.application) 
    main : L.Program Flags Model Msg Http.Error
    main =
        L.element
            { update = \msg model -> model
            , subscriptions = \model -> Sub.none
            , view =
                \model -> 
                    div []
                        [ animalView model.bird
                        , animalView model.frog
                        ]
            -- What to display to the user while the page is loading.
            , loadingView = Just (\flags -> text "Loading...")

            -- The function that takes Flags and returns a Task meant to load
            -- your initial Model and Cmd. See below.
            , load = load

            -- If the load function fails, this function will receive the
            -- error (in most cases an Http.Error) and you can display an
            -- error message to the user.
            , errorView = Just (\error -> text "Something went wrong!")

            -- You can send the error through a port if you want. This
            -- might be useful if you use an error logging service.
            , failCmd = Just (errorToString >> Ports.reportError)
            }

    
    -- You can join however many Tasks you want together to 
    -- return your starting (Model, Cmd Msg) when the page loads.
    -- Note that Task.map2, map3, etc... will load the tasks
    -- sequentially rather than in parallel. This is currently 
    -- a limitation in Elm.
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
