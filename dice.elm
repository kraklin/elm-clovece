module Dice exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, button, div, text, img, p)
import Html.App as Html
import Html.Attributes exposing (src, height)
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
    { value : Maybe Int
    , isRolling : Bool
    , rollingTime : Time
    , history : List (Maybe Int)
    }


init : Model
init =
    { value = Nothing
    , isRolling = False
    , rollingTime = 10
    , history = []
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
            let
                value = Maybe.withDefault 0 model.value
            in
                if value == 6 then
                    ( { model | value = Just 6 }, Cmd.none )
                else
                    ( { model | value = Just (value + 1) }, Cmd.none )

        Decrement ->
            let
                value = Maybe.withDefault 0 model.value
            in
            if value <= 1 then
                ( { model | value = Just 1 }, Cmd.none )
            else
                ( { model | value = Just (value - 1) }, Cmd.none )
        Roll ->
            if model.isRolling then
                ( model, Cmd.none )
            else
                ( { model | isRolling = not model.isRolling }, Cmd.none )

        GenerateNewFace time ->
            if (model.rollingTime >= 600) then
                ( { model
                    | isRolling = False
                    , rollingTime = 10
                    , history = model.value :: model.history
                    , value = Nothing
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
            ( { model | value = Just newFace }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (List.concat
            [ [ button [ onClick Decrement ] [ text "-" ]
                ,img [ src (diceImagePath model.value), onClick Roll ] []
                , button [ onClick Increment ] [ text "+" ]
              ]
            , model.history
                |> List.map
                    (\a ->
                        img [ src (diceImagePath a), height 50 ] []
                    )
            ]
        )


diceImagePath value =
    case value of
        Just val ->
            "img/" ++ toString val ++ ".png"

        Nothing ->
            "img/roll.png"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRolling then
        Time.every model.rollingTime GenerateNewFace
    else
        Sub.none
