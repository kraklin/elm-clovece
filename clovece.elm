module Main exposing (..)

import Clovece.View as View exposing (view)
import Clovece.Update as Update exposing (update, Msg)
import Clovece.Model as Model exposing (Model, init)
import Html exposing (Html, div, button, text, h1, img)


main : Program Never Model Update.Msg
main =
    Html.program
        { init = ( Model.init, Cmd.none )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Update.Msg
subscriptions model =
    Sub.none
