import Counter
import Html.App as Html
import TimeTravel.Html.App as TimeTravel
import Html exposing (Html, div)

-- import original counter from elm-architecture and try to bind the commands right
main : Program Never
main =
    TimeTravel.beginnerProgram
        { model = model
        , view = view
        , update = update
        }

type alias Model = {dice : Counter.Model}

model: Model
model = {
        dice = Counter.init
    }

type Msg =
    ChangeDice Counter.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of 
        ChangeDice msg ->
            { model | dice = Counter.update msg model.dice) }

view : Model -> Html Msg
view model =
    div []
        [ 
         Html.map ChangeDice (Counter.view model.dice)
        ]