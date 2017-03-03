module Clovece.Helpers exposing (..)


inInterval : ( Int, Int ) -> Int -> Bool
inInterval ( lower, higher ) toCheck =
    toCheck >= lower && toCheck <= higher
