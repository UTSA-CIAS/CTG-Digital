module Card exposing (..)

import Config


type alias CardId =
    Int


type Card
    = Wind
    | Food
    | Predator
    | Friend
    | LowTide
    | Competition
    | Starving
    | Rain


asList : List Card
asList =
    [ Wind
    , Food
    , Predator
    , Friend
    , LowTide
    , Competition
    , Starving
    , Rain
    ]


emoji : Card -> String
emoji card =
    case card of
        Wind ->
            "🌬"

        Food ->
            "\u{1FAB1}"

        Predator ->
            "😾"

        Friend ->
            "🐦"

        LowTide ->
            "🦐"

        Competition ->
            "🦅"

        Starving ->
            "😵\u{200D}💫"

        Rain ->
            "🌧"


name : Card -> String
name card =
    case card of
        Wind ->
            "Wind"

        Food ->
            "Food"

        Predator ->
            "Predator"

        Friend ->
            "Friend"

        LowTide ->
            "Low Tide"

        Competition ->
            "Competition"

        Starving ->
            "Starving"

        Rain ->
            "Rain"


description : Card -> String
description card =
    case card of
        Wind ->
            "Fly to the next location."

        Food ->
            "Add 1 " ++ Config.foodEmoji

        Predator ->
            "Remove 1 " ++ Config.birdEmoji

        Friend ->
            "Add 1 " ++ Config.birdEmoji

        LowTide ->
            "Add 2 " ++ Config.foodEmoji

        Competition ->
            "Remove all Food cards from the deck"

        Starving ->
            "Remove 1 " ++ Config.foodEmoji

        Rain ->
            "Remove one Wind cards from the deck"


color : Card -> String
color card =
    let
        red =
            "#f08080"

        green =
            "#57cc99"

        blue =
            "#4cc9f0"
    in
    case card of
        Wind ->
            blue

        Food ->
            green

        Predator ->
            red

        Friend ->
            green

        LowTide ->
            green

        Competition ->
            red

        Starving ->
            red

        Rain ->
            red
