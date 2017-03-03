module Clovece.Model exposing (..)

import Dict exposing (..)
import Dice exposing (..)


-- MODEL


type alias Model =
    { selectedPosition : Maybe Int
    , players : Dict String Player
    , dice : Dice.Model
    }


init : Model
init =
    { selectedPosition = Nothing
    , players =
        Dict.fromList
            [ ( "Red"
              , { name = "Red"
                , color = "#f9004e"
                , startingPosition = 0
                , finishLine = (List.range 40 43)
                , homePositions = (List.range 56 59)
                , meeplesPositions = [ 14, 57, 2, 59 ]
                }
              )
            , ( "Green"
              , { name = "Green"
                , color = "#b9d221"
                , startingPosition = 10
                , finishLine = (List.range 44 47)
                , homePositions = (List.range 60 63)
                , meeplesPositions = (List.range 60 63)
                }
              )
            , ( "Blue"
              , { name = "Blue"
                , color = "#2196d2"
                , startingPosition = 20
                , finishLine = (List.range 48 51)
                , homePositions = (List.range 68 71)
                , meeplesPositions = (List.range 68 71)
                }
              )
            , ( "Yellow"
              , { name = "Yellow"
                , color = "#ffc300"
                , startingPosition = 30
                , finishLine = (List.range 52 55)
                , homePositions = (List.range 64 67)
                , meeplesPositions = (List.range 64 67)
                }
              )
            ]
    , dice = Dice.init
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
