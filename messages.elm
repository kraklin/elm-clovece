import Counter
import Html.App as Html
import TimeTravel.Html.App as TimeTravel
import Html exposing (Html, div)

-- import original counter from elm-architecture and try to bind the commands right
main : Program Never
main =
    TimeTravel.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

type alias Model = {dice : Counter.Model}

model: Model
model = {
        dice = Counter.init
    }

init : (Model, Cmd Msg)
init = ({dice = Counter.init}, Cmd.none)

type Msg =
    ChangeDice Counter.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ChangeDice msg ->
            let (updateCounterMsg, counterCmd) = 
                Counter.update msg model.dice
            in
            ({ model | dice = updateCounterMsg}, Cmd.map ChangeDice counterCmd )

view : Model -> Html Msg
view model =
    div []
        [ 
         Html.map ChangeDice (Counter.view model.dice)
        ]