module Main exposing (..)

import Html exposing (Html, div, button, text, h1, img)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onDoubleClick, onMouseOut)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { selectedPosition : Maybe Int
    , players : List Player
    , diceValue : Int
    }


model : Model
model =
    { selectedPosition = Nothing
    , players =
        [ { name = "Red"
          , color = "#f9004e"
          , startingPosition = 1
          , finishLine = [41..44]
          , homePositions = [57..60]
          , meeplesPositions = [ 15, 58, 3, 60 ]
          }
        , { name = "Green"
          , color = "#b9d221"
          , startingPosition = 11
          , finishLine = [45..48]
          , homePositions = [61..64]
          , meeplesPositions = [61..64]
          }
        , { name = "Blue"
          , color = "#2196d2"
          , startingPosition = 21
          , finishLine = [49..52]
          , homePositions = [69..72]
          , meeplesPositions = [69..72]
          }
        , { name = "Yellow"
          , color = "#ffc300"
          , startingPosition = 31
          , finishLine = [53..56]
          , homePositions = [65..68]
          , meeplesPositions = [65..68]
          }
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
            { model
                | players =
                    List.map
                        (\player ->
                            if List.member fromPosition player.meeplesPositions then
                                { player
                                    | meeplesPositions =
                                        List.map
                                            (\m ->
                                                if m == fromPosition then
                                                    moveMeeple model.players player m model.diceValue
                                                else
                                                    m
                                            )
                                            player.meeplesPositions
                                }
                            else
                                player
                        )
                        model.players
            }

moveMeeple : List Player -> Player -> Int -> Int -> Int
moveMeeple players player position moveBy =
    let newPosition =
        getNewMeeplePosition player position moveBy
    in
        newPosition

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

kickMeeple : Player -> Int -> Int
kickMeeple player position =
    if List.member position player.homePositions then
        position
    else
        case List.head (List.filter (\home -> not (List.member home player.meeplesPositions)) player.homePositions) of
        Just newPosition -> newPosition
        Nothing -> position
    --kick

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


getXYForPosition : Int -> ( Int, Int )
getXYForPosition position =
    -- first quadrant
    if position >= 1 && position <= 4 then
        ( (position - 1), 4 )
    else if List.member position [5..9] then
        ( 4, (4 - (position - 5)) )
    else if position == 10 then
        ( 5, 0 )
        -- second quadrant
    else if List.member position [11..15] then
        ( 6, (position - 11) )
    else if List.member position [16..19] then
        ( (position - 9), 4 )
    else if position == 20 then
        ( 10, 5 )
        -- third quadrant
    else if List.member position [21..25] then
        ( (11 - (position - 20)), 6 )
    else if List.member position [26..29] then
        ( 6, (7 + position - 26) )
    else if position == 30 then
        ( 5, 10 )
        -- fourth quadrant
    else if List.member position [31..35] then
        ( 4, (10 - (position - 31)) )
    else if List.member position [36..39] then
        ( (3 - (position - 36)), 6 )
    else if position == 40 then
        ( 0, 5 )
        -- finish lines
    else if List.member position [41..44] then
        ( (1 + (position - 41)), 5 )
    else if List.member position [45..48] then
        ( 5, 1 + (position - 45) )
    else if List.member position [49..52] then
        ( (9 - (position - 49)), 5 )
    else if List.member position [53..56] then
        ( 5, 9 - (position - 53) )
        -- homes
    else if List.member position [57..60] then
        ( (position - 57) % 2, (position - 57) // 2 )
    else if List.member position [61..64] then
        ( 9 + (position - 61) % 2, (position - 61) // 2 )
    else if List.member position [65..68] then
        ( (position - 65) % 2, 9 + (position - 65) // 2 )
    else if List.member position [69..72] then
        ( 9 + (position - 69) % 2, 9 + (position - 69) // 2 )
        -- default
    else
        ( 11, 11 )


renderText : Point -> String -> Svg.Svg Msg
renderText point innerText =
    Svg.text' [ x (toString (point.x - 3)), y (toString (point.y - 3.5)), fontSize "2" ] [ Svg.text innerText ]


renderBlock : Model -> List (Svg.Svg Msg)
renderBlock model =
    List.concat
        [ List.map (\position -> (renderCircle position (getColorForPosition model.players position))) [1..72]
        , List.map (\position -> renderText (getCoordsForPosition position) (toString position)) [1..72]
        , List.concatMap (\p -> renderMeeplesForPlayer p) model.players
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text ("(elm) - Clovece, nezlob se") ]
        , svg [ viewBox "0 -5 100 110", Svg.Attributes.width "600px" ] (renderBlock model)
        ]
