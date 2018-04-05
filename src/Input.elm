module Input exposing (..)

import Keyboard exposing (KeyCode)


init : Keys
init = Keys False False False False False


keyDownSub : (KeyCode -> msg) -> Sub msg
keyDownSub = Keyboard.downs


keyUpSub : (KeyCode -> msg) -> Sub msg
keyUpSub = Keyboard.ups


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }


keyFunc : Bool -> KeyCode -> Keys -> Keys
keyFunc state keyCode keys =
    case keyCode of
        32 ->
            { keys | space = state }

        37 ->
            { keys | left = state }

        39 ->
            { keys | right = state }

        38 ->
            { keys | up = state }

        40 ->
            { keys | down = state }

        _ ->
            keys
