module Action exposing (..)

import Card exposing (Card(..), CardId)
import Deck exposing (Deck)


type Action
    = AddFoodAndThen Int Action
    | IfEnoughFoodAndThen Int (List Action) (List Action)
    | AddBirdAndThen Action
    | LooseBirdAndThen Action
    | FilterDeck (Card -> Bool)
    | DrawCard
    | Shuffle
    | RemoveDeckAndThen Action
    | ChooseNewDeck
    | NewDeck Deck
    | DiscardCard


fromCard : Card -> List Action
fromCard card =
    case card of
        Food ->
            [ AddFoodAndThen 1 DrawCard ]

        Wind ->
            [ RemoveDeckAndThen ChooseNewDeck ]

        Predator ->
            [ LooseBirdAndThen DrawCard ]

        BigPredator ->
            [ LooseBirdAndThen (LooseBirdAndThen DrawCard) ]

        Friend ->
            [ AddBirdAndThen DrawCard ]

        LowTide ->
            [ AddFoodAndThen 2 DrawCard ]

        Eagle ->
            [ FilterDeck ((/=) Food), DrawCard ]


chooseDeck : Deck -> List Action
chooseDeck deck =
    [ NewDeck deck, Shuffle, DrawCard ]


redraw : List Action
redraw =
    [ AddFoodAndThen -1 DiscardCard, DrawCard ]
