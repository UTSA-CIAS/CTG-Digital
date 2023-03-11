module Deck exposing (..)

import Card exposing (Card)


type Deck
    = Beach
    | Desert
    | Valley


asList : ( Deck, List Deck )
asList =
    ( Beach, [ Desert, Valley ] )


name : Deck -> String
name deck =
    case deck of
        Beach ->
            "Beach"

        Desert ->
            "Desert"

        Valley ->
            "Valley"


primaryColor : Deck -> String
primaryColor deck =
    case deck of
        Beach ->
            "#5C6784"

        Desert ->
            "#FCBF49"

        Valley ->
            "#7A9E9F"


secondaryColor : Deck -> String
secondaryColor deck =
    case deck of
        Beach ->
            "#98C1D9"

        Desert ->
            "#EAE2B7"

        Valley ->
            "#B8D8D8"


cards : Deck -> List Card
cards deck =
    case deck of
        Beach ->
            [ List.repeat 2 Card.Wind
            , List.repeat 6 Card.Food
            , List.repeat 2 Card.Predator
            ]
                |> List.concat

        Desert ->
            [ List.repeat 4 Card.Wind
            , List.repeat 1 Card.Food
            , List.repeat 5 Card.Predator
            ]
                |> List.concat

        Valley ->
            [ List.repeat 1 Card.Wind
            , List.repeat 5 Card.Food
            , List.repeat 4 Card.Predator
            ]
                |> List.concat


emoji : Deck -> String
emoji deck =
    case deck of
        Beach ->
            "🌊"

        Desert ->
            "🌵"

        Valley ->
            "⛰️"
