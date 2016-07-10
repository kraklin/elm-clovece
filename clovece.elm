module Main exposing (..)

import Html exposing (Html, div, button, text, h1, img)
import Html.Attributes exposing (..)
import Html.App as Html
import TimeTravel.Html.App as TimeTravel
import Html.Events exposing (onClick, onDoubleClick, onMouseOut)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (..)


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
    , diceValue : Int
    }


model : Model
model =
    { selectedPosition = Nothing
    , players =
        Dict.fromList
            [ ( "Red"
              , { name = "Red"
                , color = "#f9004e"
                , startingPosition = 1
                , finishLine = [41..44]
                , homePositions = [57..60]
                , meeplesPositions = [ 15, 58, 3, 60 ]
                }
              )
            , ( "Green"
              , { name = "Green"
                , color = "#b9d221"
                , startingPosition = 11
                , finishLine = [45..48]
                , homePositions = [61..64]
                , meeplesPositions = [61..64]
                }
              )
            , ( "Blue"
              , { name = "Blue"
                , color = "#2196d2"
                , startingPosition = 21
                , finishLine = [49..52]
                , homePositions = [69..72]
                , meeplesPositions = [69..72]
                }
              )
            , ( "Yellow"
              , { name = "Yellow"
                , color = "#ffc300"
                , startingPosition = 31
                , finishLine = [53..56]
                , homePositions = [65..68]
                , meeplesPositions = [65..68]
                }
              )
            ]
    , diceValue = 3
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
                    getToPositionForPlayer player fromPosition model.diceValue
            in
                { model
                    | players =
                        model.players
                            -- fce players -> playerName -> from -> to
                            |>
                                updatePosition fromPosition toPosition
                }


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
        (position + moveBy) > 40
    then
        (position + moveBy) % 40
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
    if position |> inInterval ( 1, 4 ) then
        ( (position - 1), 4 )
    else if position |> inInterval ( 5, 9 ) then
        ( 4, (4 - (position - 5)) )
    else if position == 10 then
        ( 5, 0 )
        -- second quadrant
    else if position |> inInterval ( 11, 15 ) then
        ( 6, (position - 11) )
    else if position |> inInterval ( 16, 19 ) then
        ( (position - 9), 4 )
    else if position == 20 then
        ( 10, 5 )
        -- third quadrant
    else if position |> inInterval ( 21, 25 ) then
        ( (11 - (position - 20)), 6 )
    else if position |> inInterval ( 26, 29 ) then
        ( 6, (7 + position - 26) )
    else if position == 30 then
        ( 5, 10 )
        -- fourth quadrant
    else if position |> inInterval ( 31, 35 ) then
        ( 4, (10 - (position - 31)) )
    else if position |> inInterval ( 36, 39 ) then
        ( (3 - (position - 36)), 6 )
    else if position == 40 then
        ( 0, 5 )
        -- finish lines
    else if position |> inInterval ( 41, 44 ) then
        ( (1 + (position - 41)), 5 )
    else if position |> inInterval ( 45, 48 ) then
        ( 5, 1 + (position - 45) )
    else if position |> inInterval ( 49, 52 ) then
        ( (9 - (position - 49)), 5 )
    else if position |> inInterval ( 53, 56 ) then
        ( 5, 9 - (position - 53) )
        -- homes
    else if position |> inInterval ( 57, 60 ) then
        ( (position - 57) % 2, (position - 57) // 2 )
    else if position |> inInterval ( 61, 64 ) then
        ( 9 + (position - 61) % 2, (position - 61) // 2 )
    else if position |> inInterval ( 65, 68 ) then
        ( (position - 65) % 2, 9 + (position - 65) // 2 )
    else if position |> inInterval ( 69, 72 ) then
        ( 9 + (position - 69) % 2, 9 + (position - 69) // 2 )
        -- default outside of visible area
    else
        ( -100, -100 )


renderText : Point -> String -> Svg.Svg Msg
renderText point innerText =
    Svg.text' [ x (toString (point.x - 3)), y (toString (point.y - 3.5)), fontSize "2" ] [ Svg.text innerText ]


renderBlock : Model -> List (Svg.Svg Msg)
renderBlock model =
    List.concat
        [ List.map (\position -> (renderCircle position (getColorForPosition (Dict.values model.players) position))) [1..72]
        , List.map (\position -> renderText (getCoordsForPosition position) (toString position)) [1..72]
        , List.concatMap (\p -> renderMeeplesForPlayer p) (Dict.values model.players)
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text ("(elm) - Clovece, nezlob se") ]
        , svg [ viewBox "0 -5 100 110", Svg.Attributes.width "600px" ] (renderBlock model)
        ]
