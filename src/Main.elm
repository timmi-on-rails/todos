module Main exposing (..)

import Backend
import Backend.WebDav
import Browser
import Html exposing (Html, a, button, div, h2, input, node, span, text)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
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
    , activeTodoId : Maybe Int
    , showSettings : Bool
    , webDavConfig : Backend.WebDav.Config
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        config =
            UserSettings.decode flags |> Result.withDefault { url = "", user = "", password = "" }

        model =
            { todos = [], activeTodoId = Nothing, showSettings = False, webDavConfig = config }
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
    | SetActiveTodo (Maybe Int)
    | ToggleSettings


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

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }
            , if model.showSettings then
                Cmd.batch
                    [ UserSettings.setWebDavConfig model.webDavConfig
                    , backend model
                        |> Result.map (.getTodos >> Task.attempt UpdateTodos)
                        |> Result.withDefault Cmd.none
                    ]

              else
                Cmd.none
            )

        WebDavConfigChanged u ->
            ( { model | webDavConfig = u }, Cmd.none )

        SetActiveTodo maybeId ->
            ( { model | activeTodoId = maybeId }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (viewNavBar
            :: (if model.showSettings then
                    [ viewConfig model.webDavConfig ]

                else
                    []
               )
            ++ [ button [ class "btn", class "btn-primary", onClick AddTodo ] [ text "add some Todo " ]
               , Keyed.node "div"
                    [ class "accordion", class "m-3" ]
                    (model.todos
                        |> List.map
                            (\todo ->
                                lazy (viewTodo (Maybe.withDefault False (Maybe.map2 (==) (Just todo.id) model.activeTodoId))) todo
                                    |> Tuple.pair (String.fromInt todo.id)
                            )
                    )
               ]
        )


viewTodo : Bool -> Todo.Todo -> Html Msg
viewTodo active todo =
    div [ class "accordion-item" ]
        [ h2 [ class "accordion-header" ]
            [ button
                [ onClick
                    (SetActiveTodo
                        (if active then
                            Nothing

                         else
                            Just todo.id
                        )
                    )
                , class "accordion-button"
                , classList [ ( "collapsed", not active ) ]
                ]
                [ text todo.title ]
            ]
        , div [ class "accordion-collapse", class "collapse", classList [ ( "show", active ) ] ]
            [ div [ class "accordion-body" ] [ text "Hallo Test" ] ]
        ]


viewNavBar : Html Msg
viewNavBar =
    node "nav"
        [ class "navbar", class "bg-body-tertiary" ]
        [ div [ class "container-fluid" ]
            [ span [ class "navbar-brand", class "mb-0", class "h1" ] [ text "Todos" ]
            , button [ type_ "button" ] [ span [ class "navbar-toggler-icon", onClick ToggleSettings ] [] ]
            ]
        ]


viewConfig : Backend.WebDav.Config -> Html Msg
viewConfig cfg =
    div []
        [ input [ placeholder "WebDAV URL", value cfg.url, onInput (\url -> WebDavConfigChanged { cfg | url = url }) ] []
        , input [ placeholder "User", value cfg.user, onInput (\usr -> WebDavConfigChanged { cfg | user = usr }) ] []
        , input [ type_ "password", placeholder "Password", value cfg.password, onInput (\pw -> WebDavConfigChanged { cfg | password = pw }) ] []
        ]
