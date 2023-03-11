module Action exposing (..)

import Card exposing (Card(..), CardId)


type Action
    = AddFoodAndThen Int Action
    | LooseBirdAndThen Action
    | DrawCard
    | Shuffle
    | EmptyDeck
    | NewDeck
    | DiscardCard


fromCard : Card -> List Action
fromCard card =
    case card of
        Food ->
            [ AddFoodAndThen 1 DrawCard ]

        Wind ->
            [ AddFoodAndThen -1 EmptyDeck, NewDeck, Shuffle, DrawCard ]

        Predator ->
            [ LooseBirdAndThen DrawCard ]


redraw : List Action
redraw =
    [ AddFoodAndThen -1 DiscardCard, DrawCard ]
