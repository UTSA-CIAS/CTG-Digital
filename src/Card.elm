module Card exposing (..)


type alias CardId =
    Int


type Card
    = Wind
    | Food
    | Predator
    | BigPredator
    | Friend
    | LowTide
    | Eagle
    | Starving


emoji : Card -> String
emoji card =
    case card of
        Wind ->
            "🌬"

        Food ->
            "\u{1FAB1}"

        Predator ->
            "😾"

        BigPredator ->
            "🦁"

        Friend ->
            "🐦"

        LowTide ->
            "🦐"

        Eagle ->
            "🦅"

        Starving ->
            "😵\u{200D}💫"


name : Card -> String
name card =
    case card of
        Wind ->
            "Wind"

        Food ->
            "Food"

        Predator ->
            "Predator"

        BigPredator ->
            "Big Predator"

        Friend ->
            "Friend"

        LowTide ->
            "Low Tide"

        Eagle ->
            "Competition"

        Starving ->
            "Starving"


description : Card -> String
description card =
    case card of
        Wind ->
            "Fly to the next location."

        Food ->
            "Add 1 Food"

        Predator ->
            "Remove 1 Bird"

        BigPredator ->
            "Remove 2 Birds"

        Friend ->
            "Add 1 Bird"

        LowTide ->
            "Add 2 Food"

        Eagle ->
            "Remove all food cards from the deck"

        Starving ->
            "Remove 1 Food"
