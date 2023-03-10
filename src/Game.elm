module Game exposing (..)

import Action exposing (Action(..))
import Card exposing (Card, CardId)
import Dict exposing (Dict)
import Random exposing (Generator)


type alias Game =
    { cards : Dict CardId Card
    , deck : List CardId
    , flying : List CardId
    , ground : Maybe CardId
    , food : Int
    , flockSize : Int
    }


initialCards : Dict CardId Card
initialCards =
    [ List.repeat 4 Card.Wind
    , List.repeat 8 Card.Food
    , List.repeat 8 Card.Predator
    ]
        |> List.concat
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


init : Game
init =
    { cards = initialCards
    , deck = List.range 0 (Dict.size initialCards - 1)
    , flying = []
    , ground = Nothing
    , food = 10
    , flockSize = 10
    }


gameOver : Game -> Bool
gameOver game =
    game.food == 0 || game.flockSize == 0


shuffle : List a -> Generator (List a)
shuffle list =
    Random.list (List.length list) (Random.float 0 1)
        |> Random.map
            (\randomList ->
                randomList
                    |> List.map2 Tuple.pair list
                    |> List.sortBy Tuple.second
                    |> List.map Tuple.first
            )


drawCard : Game -> Game
drawCard game =
    case game.deck of
        head :: tail ->
            { game | ground = Just head, deck = tail }

        [] ->
            game


applyAction : Action -> Game -> Generator Game
applyAction action game =
    case action of
        AddFood amount ->
            { game | food = game.food + amount } |> Random.constant

        DrawCard ->
            drawCard game |> Random.constant

        LooseBird ->
            { game | flockSize = game.flockSize - 1 } |> Random.constant

        Shuffle ->
            game.deck
                |> shuffle
                |> Random.map (\deck -> { game | deck = deck })

        EmptyDeck ->
            Random.constant { game | deck = [] }

        NewDeck ->
            Random.constant { game | deck = List.range 0 (Dict.size initialCards - 1) }


playCard : CardId -> Game -> Generator Game
playCard cardId game =
    game.cards
        |> Dict.get cardId
        |> Maybe.map Action.fromCard
        |> Maybe.withDefault []
        |> (::) Action.DrawCard
        |> List.foldl (\action -> Random.andThen (applyAction action))
            (Random.constant { game | flying = game.flying |> List.filter ((/=) cardId) })


getCardsFrom : Game -> List CardId -> List ( CardId, Card )
getCardsFrom game list =
    list
        |> List.filterMap
            (\cardId ->
                game.cards
                    |> Dict.get cardId
                    |> Maybe.map (Tuple.pair cardId)
            )
