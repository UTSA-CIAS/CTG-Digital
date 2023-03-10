module Action exposing (..)

import Card exposing (Card(..), CardId)


type Action
    = AddFood Int
    | LooseBird
    | DrawCard
    | Shuffle
    | EmptyDeck
    | NewDeck


fromCard : Card -> List Action
fromCard card =
    case card of
        Food ->
            [ AddFood 1 ]

        Wind ->
            [ AddFood -1, EmptyDeck, NewDeck, Shuffle ]

        Predator ->
            [ LooseBird ]
