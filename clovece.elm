module Main exposing (..)

import Html exposing (Html, div, button, text, h1, img)
import Html.Attributes exposing (..)
import Html.App as Html
import TimeTravel.Html.App as TimeTravel
import Html.Events exposing (onClick, onDoubleClick, onMouseOut)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (..)
import Counter exposing (..)


main : Program Never
main =
    TimeTravel.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { selectedPosition : Maybe Int
    , players : Dict String Player
    , dice : Counter.Model
    }


model : Model
model =
    { selectedPosition = Nothing
    , players =
        Dict.fromList
            [ ( "Red"
              , { name = "Red"
                , color = "#f9004e"
                , startingPosition = 0
                , finishLine = [40..43]
                , homePositions = [56..59]
                , meeplesPositions = [ 14, 57, 2, 59 ]
                }
              )
            , ( "Green"
              , { name = "Green"
                , color = "#b9d221"
                , startingPosition = 10
                , finishLine = [44..47]
                , homePositions = [60..63]
                , meeplesPositions = [60..63]
                }
              )
            , ( "Blue"
              , { name = "Blue"
                , color = "#2196d2"
                , startingPosition = 20
                , finishLine = [48..51]
                , homePositions = [68..71]
                , meeplesPositions = [68..71]
                }
              )
            , ( "Yellow"
              , { name = "Yellow"
                , color = "#ffc300"
                , startingPosition = 30
                , finishLine = [52..55]
                , homePositions = [64..67]
                , meeplesPositions = [64..67]
                }
              )
            ]
    , dice = Counter.init 3
    }


type alias Point =
    { x : Float
    , y : Float
    }


type alias Player =
    { name : String
    , color : String
    , startingPosition : Int
    , finishLine : List Int
    , homePositions : List Int
    , meeplesPositions : List Int
    }



-- UPDATE


type Msg
    = Move Int
    | ChangeCounter Counter.Msg

update : Msg -> Model -> Model
update msg model =
    case msg of
        Move fromPosition ->
            let
                playerName =
                    getPlayerNameForMeeplePosition model.players fromPosition

                player =
                    model.players |> Dict.get playerName

                toPosition =
                    getToPositionForPlayer player fromPosition model.dice
            in
                { model
                    | players =
                        model.players
                            -- fce players -> playerName -> from -> to
                            |>
                                updatePosition fromPosition toPosition
                }

        ChangeCounter msg ->
            { model | dice = Counter.update msg model.dice} 

getPlayerNameForMeeplePosition : Dict String Player -> Int -> String
getPlayerNameForMeeplePosition players position =
    let
        filteredPlayer =
            Dict.values players
                |> List.filter (\p -> List.member position p.meeplesPositions)
                |> List.head
    in
        case filteredPlayer of
            Just player ->
                player.name

            Nothing ->
                ""


getToPositionForPlayer : Maybe Player -> Int -> Int -> Int
getToPositionForPlayer player position moveBy =
    case player of
        Just player ->
            getNewMeeplePosition player position moveBy

        Nothing ->
            position


getNewMeeplePosition : Player -> Int -> Int -> Int
getNewMeeplePosition player position moveBy =
    -- move from home to starting position
    if List.member position player.homePositions then
        player.startingPosition
    else
    -- cross over 40
    if
        (position + moveBy) > 39
    then
        if player.name /= "Red" then
            (position + moveBy) % 40
        else
            position
    else
        position + moveBy


updatePosition : Int -> Int -> Dict String Player -> Dict String Player
updatePosition from to players =
    players
        |> Dict.map
            (\name player ->
                { player
                    | meeplesPositions =
                        player.meeplesPositions
                            |> List.map
                                (\m ->
                                    if m == from then
                                        to
                                    else if m == to then
                                        let fromTo = to
                                        in
                                        kickMeeple player to
                                    else
                                        m
                                )
                }
            )


kickMeeple : Player -> Int -> Int
kickMeeple player position =
    if List.member position player.homePositions then
        position
    else
        case List.head (List.filter (\home -> not (List.member home player.meeplesPositions)) player.homePositions) of
            Just newPosition ->
                newPosition

            Nothing ->
                position



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text ("(elm) - Clovece, nezlob se") ]
        , svg [ viewBox "0 -5 100 110", Svg.Attributes.width "600px" ] (renderBlock model)
        , Html.map ChangeCounter (Counter.view model.dice)
        ]

renderMeeple : Int -> String -> Svg.Svg Msg
renderMeeple position color =
    let
        coords =
            getCoordsForPosition position
    in
        circle
            [ coords.x |> toString |> cx
            , coords.y |> toString |> cy
            , r "2"
            , stroke color
            , fill color
            , fillOpacity "0.7"
            , strokeWidth "1.5"
            , onClick (Move position)
            ]
            []


renderMeeplesForPlayer : Player -> List (Svg.Svg Msg)
renderMeeplesForPlayer player =
    let
        color =
            player.color
    in
        List.map (\m -> renderMeeple m color) player.meeplesPositions


renderCircle : Int -> String -> Svg.Svg Msg
renderCircle position color =
    let
        coords =
            getCoordsForPosition position

        strokeParams =
            if color == "#ffffff" then
                ( "#000000", "0.1" )
            else
                ( color, "0.5" )
    in
        circle
            [ coords.x |> toString |> cx
            , coords.y |> toString |> cy
            , r "4"
            , stroke (fst strokeParams)
            , fill color
            , fillOpacity "0.2"
            , strokeWidth (snd strokeParams)
            ]
            []


getColorForPosition : List Player -> Int -> String
getColorForPosition players position =
    let
        playerOnPosition =
            players
                |> List.filter
                    (\player ->
                        player
                            |> allPlayerPositions
                            |> List.member position
                    )
                |> List.head
    in
        case playerOnPosition of
            Just player ->
                player.color

            Nothing ->
                "#ffffff"


allPlayerPositions : Player -> List Int
allPlayerPositions player =
    (player.startingPosition :: player.finishLine ++ player.homePositions)


getCoordsForPosition : Int -> Point
getCoordsForPosition position =
    let
        size =
            100 / 11

        offset =
            4.3

        xy =
            getXYForPosition position
    in
        { x = size * toFloat (fst xy) + offset, y = size * toFloat (snd xy) + offset }


inInterval : ( Int, Int ) -> Int -> Bool
inInterval interval toCheck =
    let
        lower =
            fst interval

        higher =
            snd interval
    in
        toCheck >= lower && toCheck <= higher


getXYForPosition : Int -> ( Int, Int )
getXYForPosition position =
    -- first quadrant
    if position |> inInterval ( 0, 3 ) then
        ( position, 4 )
    else if position |> inInterval ( 4, 8 ) then
        ( 4, (4 - (position - 4)) )
    else if position == 9 then
        ( 5, 0 )
        -- second quadrant
    else if position |> inInterval ( 10, 14 ) then
        ( 6, (position - 10) )
    else if position |> inInterval ( 15, 18 ) then
        ( (position - 8), 4 )
    else if position == 19 then
        ( 10, 5 )
        -- third quadrant
    else if position |> inInterval ( 20, 24 ) then
        ( (10 - (position - 20)), 6 )
    else if position |> inInterval ( 25, 28 ) then
        ( 6, (7 + position - 25) )
    else if position == 29 then
        ( 5, 10 )
        -- fourth quadrant
    else if position |> inInterval ( 30, 34 ) then
        ( 4, (10 - (position - 30)) )
    else if position |> inInterval ( 35, 38 ) then
        ( (3 - (position - 35)), 6 )
    else if position == 39 then
        ( 0, 5 )
        -- finish lines
    else if position |> inInterval ( 40, 43 ) then
        ( (1 + (position - 40)), 5 )
    else if position |> inInterval ( 44, 47 ) then
        ( 5, 1 + (position - 44) )
    else if position |> inInterval ( 48, 51 ) then
        ( (9 - (position - 48)), 5 )
    else if position |> inInterval ( 52, 55 ) then
        ( 5, 9 - (position - 52) )
        -- homes
    else if position |> inInterval ( 56, 59 ) then
        ( (position - 56) % 2, (position - 56) // 2 )
    else if position |> inInterval ( 60, 63 ) then
        ( 9 + (position - 60) % 2, (position - 60) // 2 )
    else if position |> inInterval ( 64, 67 ) then
        ( (position - 64) % 2, 9 + (position - 64) // 2 )
    else if position |> inInterval ( 68, 71 ) then
        ( 9 + (position - 68) % 2, 9 + (position - 68) // 2 )
        -- default outside of visible area
    else
        ( -100, -100 )


renderText : Point -> String -> Svg.Svg Msg
renderText point innerText =
    Svg.text' [ x (toString (point.x - 3)), y (toString (point.y - 3.5)), fontSize "2" ] [ Svg.text innerText ]


renderBlock : Model -> List (Svg.Svg Msg)
renderBlock model =
    List.concat
        [ List.map (\position -> (renderCircle position (getColorForPosition (Dict.values model.players) position))) [0..71]
        , List.map (\position -> renderText (getCoordsForPosition position) (toString position)) [0..71]
        , List.concatMap (\p -> renderMeeplesForPlayer p) (Dict.values model.players)
        ]
