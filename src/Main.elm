module Main exposing (..)

import Backend
import Backend.WebDav
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Encode as E
import Task
import Todo
import UserSettings



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { todos : List Todo.Todo
    , webDavConfig : Backend.WebDav.Config
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        config =
            UserSettings.decode flags |> Result.withDefault { url = "", user = "", password = "" }

        model =
            { todos = [], webDavConfig = config }
    in
    ( model, backend model |> .getTodos |> Task.attempt UpdateTodos )


backend : Model -> Backend.Backend
backend model =
    Backend.WebDav.build model.webDavConfig



-- UPDATE


type Msg
    = UpdateTodos (Result String (List Todo.Todo))
    | AddTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            ( model, (backend model |> .addTodo) "test" |> Task.attempt UpdateTodos )

        UpdateTodos res ->
            let
                s =
                    case res of
                        Ok a ->
                            a

                        Err e ->
                            [ { id = 0, title = e } ]
            in
            ( { model | todos = s }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddTodo ] [ text "add some Todo " ]
        , div [] (model.todos |> List.map (\todo -> div [] [ text todo.title ]))
        ]
