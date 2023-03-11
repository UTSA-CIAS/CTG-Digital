module Game exposing (..)

import Action exposing (Action(..))
import Card exposing (Card, CardId)
import Dict exposing (Dict)
import Random exposing (Generator)


type alias Game =
    { cards : Dict CardId Card
    , deck : List CardId
    , ground : Maybe CardId
    , food : Int
    , flockSize : Int
    }


initialCards : Dict CardId Card
initialCards =
    [ List.repeat 2 Card.Wind
    , List.repeat 4 Card.Food
    , List.repeat 4 Card.Predator
    ]
        |> List.concat
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


init : Game
init =
    { cards = initialCards
    , deck = List.range 0 (Dict.size initialCards - 1)
    , ground = Nothing
    , food = 10
    , flockSize = 10
    }


gameOver : Game -> Bool
gameOver game =
    game.food < 0 || game.flockSize == 0


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
        AddFoodAndThen amount action2 ->
            { game | food = game.food + amount }
                |> applyAction action2

        DrawCard ->
            drawCard game |> Random.constant

        LooseBirdAndThen action2 ->
            { game | flockSize = game.flockSize - 1 }
                |> applyAction action2

        Shuffle ->
            game.deck
                |> shuffle
                |> Random.map (\deck -> { game | deck = deck })

        EmptyDeck ->
            Random.constant { game | deck = [] }

        NewDeck ->
            Random.constant { game | deck = List.range 0 (Dict.size initialCards - 1) }

        DiscardCard ->
            { game
                | deck = game.deck ++ (game.ground |> Maybe.map List.singleton |> Maybe.withDefault [])
                , ground = Nothing
            }
                |> Random.constant


playCard : CardId -> Game -> Generator Game
playCard cardId game =
    game.cards
        |> Dict.get cardId
        |> Maybe.map Action.fromCard
        |> Maybe.withDefault []
        |> List.foldl (\action -> Random.andThen (applyAction action))
            (Random.constant game)


getCardsFrom : Game -> List CardId -> List ( CardId, Card )
getCardsFrom game list =
    list
        |> List.filterMap
            (\cardId ->
                game.cards
                    |> Dict.get cardId
                    |> Maybe.map (Tuple.pair cardId)
            )
