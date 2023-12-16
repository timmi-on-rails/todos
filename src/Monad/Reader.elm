module Monad.Reader exposing (Reader, ask, map, run)

import Monad.State as State


type Reader error env a
    = Reader (State.State error env a)


run : env -> Reader error env a -> Result error a
run env (Reader r) =
    State.run env r |> Tuple.first


ask : Reader error env env
ask =
    Reader State.get


map : (a -> b) -> Reader error env a -> Reader error env b
map f (Reader r) =
    Reader <| State.map f r
