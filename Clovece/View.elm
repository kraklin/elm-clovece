module Clovece.View exposing (view)

import Html exposing (Html, div, button, text, h1, img)
import Html.Events exposing (onClick, onDoubleClick, onMouseOut)
import Dice exposing (view)
import Dict exposing (values)
import Clovece.Model exposing (..)
import Clovece.Helpers exposing (inInterval)
import Clovece.Update exposing (Msg)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text ("(elm) - Clovece, nezlob se") ]
        , svg [ viewBox "0 -5 100 110", Svg.Attributes.width "600px" ] (renderBlock model)
        , Html.map Clovece.Update.ChangeDice (Dice.view model.dice)
        ]


renderMeeple : Int -> Player -> Svg.Svg Msg
renderMeeple position player =
    let
        coords =
            getCoordsForPosition position
    in
        circle
            [ coords.x |> toString |> cx
            , coords.y |> toString |> cy
            , r "2"
            , stroke player.color
            , fill player.color
            , fillOpacity "0.7"
            , strokeWidth "1.5"
            , onClick (Clovece.Update.Move position player)
            ]
            []


renderMeeplesForPlayer : Player -> List (Svg.Svg Msg)
renderMeeplesForPlayer player =
    player.meeplesPositions
        |> List.map (\m -> renderMeeple m player)


renderCircle : Int -> String -> Svg.Svg Msg
renderCircle position color =
    let
        coords =
            getCoordsForPosition position

        ( strokeCol, strokeWdth ) =
            if color == "#ffffff" then
                ( "#000000", "0.1" )
            else
                ( color, "0.5" )
    in
        circle
            [ coords.x |> toString |> cx
            , coords.y |> toString |> cy
            , r "4"
            , stroke strokeCol
            , fill color
            , fillOpacity "0.2"
            , strokeWidth strokeWdth
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

        ( xCoord, yCoord ) =
            getXYForPosition position
    in
        { x = size * toFloat xCoord + offset, y = size * toFloat yCoord + offset }


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
    -- Svg.text [ x (toString (point.x - 3)), y (toString (point.y - 3.5)), fontSize "2" ] [ Svg.text innerText ]
    Svg.text innerText


renderBlock : Model -> List (Svg.Svg Msg)
renderBlock model =
    List.concat
        [ List.map (\position -> (renderCircle position (getColorForPosition (Dict.values model.players) position))) (List.range 0 71)
        , List.map (\position -> renderText (getCoordsForPosition position) (toString position)) (List.range 0 71)
        , (Dict.values model.players |> List.concatMap (\p -> renderMeeplesForPlayer p))
        ]
