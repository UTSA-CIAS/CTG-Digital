module Deck exposing (..)

import Card exposing (Card)


type Deck
    = Beach
    | Desert
    | Valley
    | Savanna
    | Island
    | Jungle


asList : List Deck
asList =
    [ Beach, Desert, Valley, Savanna, Island, Jungle ]


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

        Island ->
            "Island"

        Jungle ->
            "Jungle"


primaryColor : Deck -> String
primaryColor deck =
    case deck of
        Island ->
            "#70d6ff"

        Desert ->
            "#ffdc5e"

        Valley ->
            "#a7c957"

        Savanna ->
            "#ffbf81"

        Beach ->
            "#FFEEDD"

        Jungle ->
            --todo: change
            "#a7c957"


cards : Deck -> List Card
cards deck =
    case deck of
        Beach ->
            [ List.repeat 2 Card.Wind
            , List.repeat 1 Card.Rain
            , List.repeat 5 Card.Predator
            , List.repeat 2 Card.LowTide
            ]
                |> List.concat

        Desert ->
            [ List.repeat 3 Card.Wind
            , List.repeat 3 Card.Predator
            , List.repeat 3 Card.Starving
            , List.repeat 1 Card.Friend
            ]
                |> List.concat

        Valley ->
            [ List.repeat 1 Card.Wind
            , List.repeat 4 Card.Food
            , List.repeat 4 Card.Predator
            , List.repeat 1 Card.Competition
            ]
                |> List.concat

        Savanna ->
            [ List.repeat 1 Card.Wind
            , List.repeat 3 Card.Food
            , List.repeat 5 Card.Predator
            , List.repeat 1 Card.Friend
            ]
                |> List.concat

        Island ->
            [ List.repeat 2 Card.Wind
            , List.repeat 3 Card.Starving
            , List.repeat 3 Card.LowTide
            , List.repeat 2 Card.Predator
            ]
                |> List.concat

        Jungle ->
            [ List.repeat 3 Card.Wind
            , List.repeat 2 Card.Rain
            , List.repeat 2 Card.Food
            , List.repeat 3 Card.Predator
            ]
                |> List.concat


emoji : Deck -> String
emoji deck =
    case deck of
        Beach ->
            "🦀"

        Desert ->
            "🐫"

        Valley ->
            "🐐"

        Savanna ->
            "🦒"

        Island ->
            "🐬"

        Jungle ->
            "🦜"
