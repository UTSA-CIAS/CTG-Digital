module Event exposing (..)

import Action exposing (Action)
import Deck exposing (Deck)


type Event
    = AddActions (List Action)
    | ChooseDeck (List Deck)
