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


description : Card -> String
description card =
    case card of
        Wind ->
            "Fly to the next location."

        Food ->
            "Add 1 " ++ Config.foodEmoji ++ "Food"

        Predator ->
            "Remove 1 " ++ Config.birdEmoji ++ "Bird"

        Friend ->
            "Add 1 " ++ Config.birdEmoji ++ "Bird"

        LowTide ->
            "Add 2 " ++ Config.foodEmoji ++ "Food"

        Competition ->
            "Remove all " ++ Config.foodEmoji ++ " food cards from the deck"

        Starving ->
            "Remove 1 " ++ Config.foodEmoji ++ "Food"
