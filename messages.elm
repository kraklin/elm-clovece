import Dice
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
    Sub.batch [
    Sub.map ChangeDice (Dice.subscriptions model.dice)
    ]

type alias Model = {dice : Dice.Model}

model: Model
model = {
        dice = Dice.init
    }

init : (Model, Cmd Msg)
init = ({dice = Dice.init}, Cmd.none)

type Msg =
    ChangeDice Dice.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ChangeDice msg ->
            let (updateDiceMsg, diceCmd) = 
                Dice.update msg model.dice
            in
            ({ model | dice = updateDiceMsg}, Cmd.map ChangeDice diceCmd )

view : Model -> Html Msg
view model =
    div []
        [ 
         Html.map ChangeDice (Dice.view model.dice)
        ]