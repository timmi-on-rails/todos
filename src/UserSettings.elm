port module UserSettings exposing (decode, setWebDavConfig)

import Backend.WebDav
import Json.Decode as D
import Json.Encode as E


encode : Backend.WebDav.Config -> E.Value
encode cfg =
    E.object
        [ ( "url", E.string cfg.url )
        , ( "user", E.string cfg.user )
        , ( "password", E.string cfg.password )
        ]


decode : E.Value -> Result String Backend.WebDav.Config
decode value =
    case D.decodeValue decoder value of
        Ok cfg ->
            Ok cfg

        Err err ->
            Err <| D.errorToString err


decoder : D.Decoder Backend.WebDav.Config
decoder =
    D.map3 Backend.WebDav.Config
        (D.field "url" D.string)
        (D.field "user" D.string)
        (D.field "password" D.string)


setWebDavConfig : Backend.WebDav.Config -> Cmd msg
setWebDavConfig cfg =
    setWebDavConfigInternal (encode cfg)


port setWebDavConfigInternal : E.Value -> Cmd msg
