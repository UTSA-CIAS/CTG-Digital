module Event exposing (..)

import Action exposing (Action)
import Deck exposing (Deck)


type Event
    = AddActions (List Action)
    | ChooseDeck (List Deck)
    | PlaySound Sound


type Sound
    = Shuffle
    | Draw
    | TakeOff
    | Discard
    | Loose
    | Win
    | AddBird


toString : Sound -> String
toString sound =
    case sound of
        Shuffle ->
            "Shuffle"

        Draw ->
            "Draw"

        TakeOff ->
            "TakeOff"

        Discard ->
            "Discard"

        Loose ->
            "Loose"

        Win ->
            "Win"

        AddBird ->
            "AddBird"


sounds : List ( String, String )
sounds =
    [ ( "shuffle.wav", toString Shuffle )
    , ( "playcard.wav", toString Draw )
    , ( "flying.mp3", toString TakeOff )
    , ( "draw.wav", toString Discard )
    , ( "error.wav", toString Loose )
    , ( "amb_bird_2.mp3", toString Win )
    , ( "bird.mp3", toString AddBird )
    ]
