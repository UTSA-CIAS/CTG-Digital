module Action exposing (..)

import Card exposing (Card(..), CardId)
import Deck exposing (Deck)


type Action
    = AddFoodAndThen Int Action
    | IfEnoughFoodAndThen Int (List Action) (List Action)
    | LooseBirdAndThen Action
    | DrawCard
    | Shuffle
    | RemoveDeck
    | NewDeck Deck
    | DiscardCard


fromCard : Card -> List Action
fromCard card =
    case card of
        Food ->
            [ AddFoodAndThen 1 DrawCard ]

        Wind ->
            [ RemoveDeck ]

        Predator ->
            [ LooseBirdAndThen DrawCard ]


chooseDeck : Deck -> List Action
chooseDeck deck =
    [ NewDeck deck, Shuffle, DrawCard ]


redraw : List Action
redraw =
    [ AddFoodAndThen -1 DiscardCard, DrawCard ]
