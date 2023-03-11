module Deck exposing (..)

import Card exposing (Card)


type Deck
    = Beach
    | Desert
    | Valley
    | Savanna


asList : List Deck
asList =
    [ Beach, Desert, Valley, Savanna ]


name : Deck -> String
name deck =
    case deck of
        Beach ->
            "Beach"

        Desert ->
            "Desert"

        Valley ->
            "Valley"

        Savanna ->
            "Savanna"


primaryColor : Deck -> String
primaryColor deck =
    case deck of
        Beach ->
            "#5C6784"

        Desert ->
            "#FCBF49"

        Valley ->
            "#7A9E9F"

        Savanna ->
            "#e9afa3"


secondaryColor : Deck -> String
secondaryColor deck =
    case deck of
        Beach ->
            "#98C1D9"

        Desert ->
            "#EAE2B7"

        Valley ->
            "#B8D8D8"

        Savanna ->
            "#f9dec9"


cards : Deck -> List Card
cards deck =
    case deck of
        Beach ->
            [ List.repeat 2 Card.Wind
            , List.repeat 1 Card.Food
            , List.repeat 5 Card.Predator
            , List.repeat 2 Card.LowTide
            ]
                |> List.concat

        Desert ->
            [ List.repeat 3 Card.Wind
            , List.repeat 5 Card.Predator
            , List.repeat 1 Card.BigPredator
            , List.repeat 1 Card.Friend
            ]
                |> List.concat

        Valley ->
            [ List.repeat 1 Card.Wind
            , List.repeat 4 Card.Food
            , List.repeat 4 Card.Predator
            , List.repeat 1 Card.Eagle
            ]
                |> List.concat

        Savanna ->
            [ List.repeat 1 Card.Wind
            , List.repeat 3 Card.Food
            , List.repeat 5 Card.Predator
            , List.repeat 1 Card.Friend
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

        Savanna ->
            "🦓"
