module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Persistence
import Task
import Todo



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( init, Persistence.getTodos |> Task.attempt UpdateTodos )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    List Todo.Todo


init : Model
init =
    []



-- UPDATE


type Msg
    = UpdateTodos (Result String (List Todo.Todo))
    | AddTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            ( model, Persistence.addTodo "test" |> Task.attempt UpdateTodos )

        UpdateTodos res ->
            let
                s =
                    case res of
                        Ok a ->
                            a

                        Err e ->
                            [ { id = 0, title = e } ]
            in
            ( s, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddTodo ] [ text "add some Todo" ]
        , div [] (model |> List.map (\todo -> div [] [ text todo.title ]))
        ]
