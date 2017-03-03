module Clovece.Update exposing (..)

import Clovece.Model exposing (Player, Model)
import Dice exposing (Msg)
import Dict exposing (..)
import Clovece.Helpers exposing (inInterval)


-- UPDATE


type Msg
    = Move Int Player
    | ChangeDice Dice.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move fromPosition player ->
            let
                toPosition =
                    getNewMeeplePosition player fromPosition model.dice
            in
                ( { model
                    | players =
                        model.players
                            |> updatePosition fromPosition toPosition
                  }
                , Cmd.none
                )

        ChangeDice msg ->
            let
                ( diceUpd, diceCmd ) =
                    Dice.update msg model.dice
            in
                ( { model | dice = diceUpd }, Cmd.map ChangeDice diceCmd )


getNewMeeplePosition : Player -> Int -> Int -> Int
getNewMeeplePosition player position moveBy =
    let
        newTranslatedPos =
            (toOrigin player.startingPosition position) + moveBy
    in
        -- dont move when on finish line
        if List.member position player.finishLine then
            position
            -- move from home to starting position
        else if List.member position player.homePositions then
            if moveBy == 6 then
                player.startingPosition
            else
                position
            -- unless you reach the end of standard plane, move
        else if newTranslatedPos <= 39 then
            fromOrigin player.startingPosition newTranslatedPos
        else if newTranslatedPos |> inInterval ( 40, 43 ) then
            -- go to finish line
            (newTranslatedPos - 40) + (firstHomePosition player)
        else
            position


toOrigin : Int -> Int -> Int
toOrigin startingPosition position =
    (position - startingPosition) % 40


fromOrigin : Int -> Int -> Int
fromOrigin startingPosition translatedPos =
    (translatedPos + startingPosition) % 40


firstHomePosition : Player -> Int
firstHomePosition { finishLine } =
    (List.head finishLine) |> Maybe.withDefault -100


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
