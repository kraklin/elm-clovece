module Dice exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, text, img)
import Html.App as Html
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random
import Time exposing (Time, second)


main : Program Never
main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { value : Int
    , isRolling : Bool
    , rollingTime : Time
    }


init : Model
init =
    { value = 1
    , isRolling = False
    , rollingTime = 10
    }
 


-- UPDATE


type Msg
    = Increment
    | Decrement
    | Roll
    | GenerateNewFace Time
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            if model.value == 6 then
                ( { model | value = 6 }, Cmd.none )
            else
                ( { model | value = model.value + 1 }, Cmd.none )

        Decrement ->
            if model.value == 1 then
                ( { model | value = 1 }, Cmd.none )
            else
                ( { model | value = model.value - 1 }, Cmd.none )

        Roll ->
            ( { model | isRolling = not model.isRolling }, Cmd.none )

        GenerateNewFace time ->
            if (model.rollingTime >= 600) then
                ( { model
                    | isRolling = False
                    , rollingTime = 10
                  }
                , Cmd.none
                )
            else
                ( { model
                    | rollingTime = model.rollingTime * 2
                  }
                , Random.generate NewFace (Random.int 1 6)
                )

        NewFace newFace ->
            ( { model | value = newFace }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , img [ src ("img/" ++ toString model.value ++ ".png"), onClick Roll ] []
        , button [ onClick Increment ] [ text "+" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRolling then
        Time.every model.rollingTime GenerateNewFace
    else
        Sub.none
