port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Persistence
import Task
import Todo



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
    , webDavConfig : Maybe WebDavConfig
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( let
        model =
            { todos = [], webDavConfig = Nothing }
      in
      case D.decodeValue decoder flags of
        Ok cfg ->
            { model | webDavConfig = Just cfg }

        Err _ ->
            model
    , Persistence.getTodos |> Task.attempt UpdateTodos
    )


type alias WebDavConfig =
    { url : String
    , user : String
    , password : String
    }


encode : WebDavConfig -> E.Value
encode cfg =
    E.object
        [ ( "url", E.string cfg.url )
        , ( "user", E.string cfg.user )
        , ( "password", E.string cfg.password )
        ]


decoder : D.Decoder WebDavConfig
decoder =
    D.map3 WebDavConfig
        (D.field "url" D.string)
        (D.field "user" D.string)
        (D.field "password" D.string)


port setWebDavConfig : E.Value -> Cmd msg



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
            ( { model | todos = s }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddTodo ]
            [ text
                ("add some Todo "
                    ++ (model.webDavConfig
                            |> Maybe.map
                                (\cfg ->
                                    cfg.user ++ " " ++ cfg.password ++ " " ++ cfg.url
                                )
                            |> Maybe.withDefault ""
                       )
                )
            ]
        , div [] (model.todos |> List.map (\todo -> div [] [ text todo.title ]))
        ]
