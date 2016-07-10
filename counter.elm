module Counter exposing ( Model, Msg, init, update, view )

import Html exposing (Html, button, div, text, img)
import Html.App as Html
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)




-- MODEL


type alias Model = Int

init : Int -> Model
init count =
  count



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      if model == 6 then 6 else model + 1

    Decrement ->
      if model == 1 then 1 else model - 1



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , img [src ("img/" ++ toString model ++ ".png")] [  ]
    , button [ onClick Increment ] [ text "+" ]
    ]