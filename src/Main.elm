module Main exposing (..)

import Backend
import Backend.WebDav
import Browser
import Html exposing (Html, button, div, input, node, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
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
    ( model
    , backend model
        |> Result.map (.getTodos >> Task.attempt UpdateTodos)
        |> Result.withDefault Cmd.none
    )


backend : Model -> Result String Backend.Backend
backend model =
    Backend.WebDav.build model.webDavConfig



-- UPDATE


type Msg
    = UpdateTodos (Result String (List Todo.Todo))
    | AddTodo
    | WebDavConfigChanged Backend.WebDav.Config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            ( model
            , backend model
                |> Result.map
                    (\b ->
                        b.addTodo "test"
                            |> Task.attempt UpdateTodos
                    )
                |> Result.withDefault Cmd.none
            )

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

        WebDavConfigChanged u ->
            ( { model | webDavConfig = u }, UserSettings.setWebDavConfig u )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNavBar
        , viewConfig model.webDavConfig
        , button [ class "btn", class "btn-primary", onClick AddTodo ] [ text "add some Todo " ]
        , div [] (model.todos |> List.map (\todo -> div [] [ text todo.title ]))
        ]


viewNavBar : Html Msg
viewNavBar =
    node "nav"
        [ class "navbar", class "bg-body-tertiary" ]
        [ div [ class "container-fluid" ]
            [ span [ class "navbar-brand", class "mb-0", class "h1" ] [ text "Todos" ] ]
        ]


viewConfig : Backend.WebDav.Config -> Html Msg
viewConfig cfg =
    div []
        [ input [ placeholder "WebDAV URL", value cfg.url, onInput (\url -> WebDavConfigChanged { cfg | url = url }) ] []
        , input [ placeholder "User", value cfg.user, onInput (\usr -> WebDavConfigChanged { cfg | user = usr }) ] []
        , input [ type_ "password", placeholder "Password", value cfg.password, onInput (\pw -> WebDavConfigChanged { cfg | password = pw }) ] []
        ]
