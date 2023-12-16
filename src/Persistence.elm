module Persistence exposing (addTodo, getTodos)

import Base64
import Csv.Decode as Decode exposing (Decoder)
import Csv.Encode as Encode
import Dict
import Http
import Monad.Reader as Reader
import Monad.State as State
import Retry
import Task
import Todo


readTodos : Reader.Reader String (List Todo.Todo) a -> Task.Task String a
readTodos r =
    getTodosHttp
        |> Task.map .todos
        |> Task.andThen
            (\todos ->
                case r |> Reader.run todos of
                    Ok v ->
                        Task.succeed v

                    Err e ->
                        Task.fail e
            )


modifyTodos : State.State String (List Todo.Todo) a -> Task.Task String a
modifyTodos r =
    (Retry.retryIf Tuple.first <|
        (getTodosHttp
            |> Task.andThen
                (\x ->
                    case r |> State.run x.todos of
                        ( Ok v, s ) ->
                            Task.succeed { etag = x.etag, todos = s, value = v }

                        ( Err e, _ ) ->
                            Task.fail e
                )
            |> Task.mapError (\e -> ( False, e ))
            |> Task.andThen
                (\x -> putTodosHttp { etag = x.etag, todos = x.todos } |> Task.map (\_ -> x.value))
        )
    )
        |> Task.mapError Tuple.second


getTodosHttp : Task.Task String { etag : String, todos : List Todo.Todo }
getTodosHttp =
    Http.task
        { url = "https://seafile.coding-tom.de/seafdav/Dokumente/Test.txt"
        , method = "GET"
        , body = Http.emptyBody
        , headers =
            [ buildAuthorizationHeader "u" "p"
            , Http.header "Cache-Control" "max-age=0,no-cache,no-store,post-check=0,pre-check=0"
            ]
        , resolver =
            Http.stringResolver
                (\y ->
                    case y of
                        Http.GoodStatus_ m b ->
                            decode b
                                |> Result.andThen
                                    (\x ->
                                        m.headers
                                            |> Dict.get "etag"
                                            |> Result.fromMaybe "missing header value"
                                            |> Result.map (\z -> { etag = z, todos = x })
                                    )

                        Http.BadStatus_ _ b ->
                            Err b

                        Http.BadUrl_ u ->
                            Err u

                        _ ->
                            Err "error"
                )
        , timeout = Nothing
        }


putTodosHttp : { etag : String, todos : List Todo.Todo } -> Task.Task ( Bool, String ) ()
putTodosHttp x =
    Http.task
        { url = "https://seafile.coding-tom.de/seafdav/Dokumente/Test.txt"
        , method = "PUT"
        , body =
            Http.stringBody "text/plain"
                (x.todos
                    |> Encode.encode
                        { encoder =
                            Encode.withFieldNames
                                (\todo ->
                                    [ ( "id", String.fromInt todo.id )
                                    , ( "title", todo.title )
                                    ]
                                )
                        , fieldSeparator = ','
                        }
                )
        , headers =
            [ buildAuthorizationHeader "u" "p"
            , Http.header "If-Match" x.etag
            ]
        , resolver =
            Http.stringResolver
                (\y ->
                    case y of
                        Http.GoodStatus_ _ _ ->
                            Ok ()

                        Http.BadStatus_ s _ ->
                            Err <| ( s.statusCode == 412, String.fromInt s.statusCode )

                        Http.BadUrl_ u ->
                            Err ( False, u )

                        Http.Timeout_ ->
                            Err ( False, "timeout" )

                        Http.NetworkError_ ->
                            Err ( False, "network error" )
                )
        , timeout = Nothing
        }


getTodos : Task.Task String (List Todo.Todo)
getTodos =
    Reader.ask |> readTodos


addTodo : String -> Task.Task String (List Todo.Todo)
addTodo title =
    State.get
        |> State.andThen (\todos -> State.put <| { id = 2, title = title } :: todos)
        |> State.andThen (\_ -> State.get)
        |> modifyTodos


decoder : Decoder Todo.Todo
decoder =
    Decode.into Todo.Todo
        |> Decode.pipeline (Decode.field "id" Decode.int)
        |> Decode.pipeline (Decode.field "title" Decode.string)


decode : String -> Result String (List Todo.Todo)
decode csv =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv
        |> Result.mapError
            (\e ->
                case e of
                    Decode.ParsingError _ ->
                        "parsing error"

                    Decode.NoFieldNamesOnFirstRow ->
                        "no field names"

                    Decode.DecodingErrors _ ->
                        "decoding errors"
            )


buildAuthorizationHeader : String -> String -> Http.Header
buildAuthorizationHeader username password =
    Http.header "Authorization" ("Basic " ++ Base64.encode (username ++ ":" ++ password))
