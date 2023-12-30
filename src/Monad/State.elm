module Monad.State exposing (State, state, andThen, get, map, put, run)


type State error state value
    = State (state -> ( Result error value, state ))


{-| Create a new State from a value of any type.
-}
state : value -> State error state value
state value =
    State (\s -> ( Ok value, s ))


{-| Get the current state. Typically the state is
modified somehow and then put back with put.
-}
get : State error s s
get =
    State (\s -> ( Ok s, s ))


{-| Replace the current state with a new one.
-}
put : s -> State error s ()
put x =
    State (\_ -> ( Ok (), x ))


{-| Apply a function to the value that the state holds
-}
map : (a -> b) -> State error s a -> State error s b
map f (State step) =
    State <|
        \currentState ->
            let
                ( value, newState ) =
                    step currentState
            in
            ( value |> Result.map f, newState )


andThen : (a -> State error s b) -> State error s a -> State error s b
andThen f (State h) =
    State <|
        \s ->
            let
                ( a, newState ) =
                    h s
            in
            case a of
                Ok k ->
                    let
                        (State g) =
                            f k
                    in
                    g newState

                Err e ->
                    ( Err e, newState )


run : s -> State error s a -> ( Result error a, s )
run initialState s =
    let
        (State f) =
            s
    in
    f initialState
