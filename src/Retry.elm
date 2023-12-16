module Retry exposing (Config, default, retryIf, retryIfWith)

import Process
import Task exposing (Task)


type alias Config =
    { retries : Int
    , interval : Float
    }


default : Config
default =
    { retries = 1
    , interval = 1000
    }


retryIf : (x -> Bool) -> Task x a -> Task x a
retryIf =
    retryIfWith default


retryIfWith : Config -> (x -> Bool) -> Task x a -> Task x a
retryIfWith config cond task =
    task
        |> Task.onError
            (\x ->
                if cond x then
                    onError config task x

                else
                    Task.fail x
            )


onError : Config -> Task x a -> x -> Task x a
onError config task error =
    if config.retries == 0 then
        Task.fail error

    else
        let
            next =
                task
                    |> Task.onError (onError { config | retries = config.retries - 1 } task)
        in
        Process.sleep config.interval
            |> Task.andThen (always next)
