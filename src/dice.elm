module Dice exposing (..)

import Html exposing (Html, button, div, text, img)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random
import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Int


init : Model
init =
    (1)



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            if model == 6 then
                ( 6, Cmd.none )
            else
                ( model + 1, Cmd.none )

        Decrement ->
            if model == 1 then
                ( 1, Cmd.none )
            else
                ( model - 1, Cmd.none )

        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace newFace ->
            ( Debug.log "newFace:" newFace, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , img [ src ("../img/" ++ toString model ++ ".png"), onClick Roll ] []
        , button [ onClick Increment ] [ text "+" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
