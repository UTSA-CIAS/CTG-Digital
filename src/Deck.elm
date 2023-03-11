module Deck exposing (..)

import Card exposing (Card)


type Deck
    = Beach
    | Desert
    | Valley


asList : List Deck
asList =
    [ Beach, Desert, Valley ]


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
            "#92DCE5"

        Desert ->
            "#FCBF49"

        Valley ->
            "#6C91BF"


secondaryColor : Deck -> String
secondaryColor deck =
    case deck of
        Beach ->
            "#F8F7F9"

        Desert ->
            "#EAE2B7"

        Valley ->
            "#5FB0B7"


cards : Deck -> List Card
cards deck =
    case deck of
        Beach ->
            [ List.repeat 4 Card.Wind
            , List.repeat 5 Card.Food
            , List.repeat 1 Card.Predator
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
