module View exposing (..)

import Card exposing (Card, CardId)
import Config
import Dict
import Game exposing (Game)
import Game.Area
import Game.Card
import Game.Entity
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


viewCard : List (Html.Attribute msg) -> Bool -> Card -> Game.Entity.Entity (List (Html.Attribute msg) -> Html msg)
viewCard attributes faceUp c =
    Game.Entity.flippable
        ([ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px")
         , Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px")
         ]
            ++ attributes
        )
        { front =
            (\attrs ->
                [ Card.name c |> Html.text |> Game.Card.element []
                , Card.emoji c |> Html.text |> Game.Card.element [ Html.Attributes.style "font-size" "40px" ]
                ]
                    |> Game.Card.default (attrs ++ [ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px") ])
            )
                |> Game.Entity.new
        , back =
            (\attrs ->
                Layout.none
                    |> Game.Card.back (attrs ++ [ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px"), Html.Attributes.class "cardBack" ])
            )
                |> Game.Entity.new
        , faceUp = faceUp
        }


viewEmptyCard : String -> ( String, List (Attribute msg) -> Html msg )
viewEmptyCard name =
    ( name
    , \attrs ->
        Game.Card.empty
            (attrs ++ [ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px") ])
            name
    )


viewGame : { selectCard : CardId -> msg } -> Game -> Html msg
viewGame args game =
    [ Html.text "Waiting for Wind" |> Layout.el []
    , [ game.flying
            |> Game.getCardsFrom game
            |> List.map
                (\( cardId, card ) ->
                    card
                        |> viewCard
                            (Layout.asButton
                                { onPress = Just (args.selectCard cardId)
                                , label = "Select " ++ Card.name card
                                }
                            )
                            True
                        |> Game.Entity.map (Tuple.pair (String.fromInt cardId))
                )
            |> Game.Area.mapPosition (\i _ -> Tuple.mapFirst ((+) (toFloat i * (Config.spacing * Config.cardWidth) + Config.spacing + Config.cardWidth)))
            |> Game.Area.mapZIndex (\_ _ -> (+) 50)
      , game.ground
            |> Maybe.andThen (\cardId -> Dict.get cardId game.cards |> Maybe.map (Tuple.pair cardId))
            |> Maybe.map
                (\( cardId, card ) ->
                    card
                        |> viewCard
                            (Layout.asButton
                                { onPress = Just (args.selectCard cardId)
                                , label = "Select " ++ Card.name card
                                }
                            )
                            True
                        |> Game.Entity.map (Tuple.pair (String.fromInt cardId))
                        |> Game.Entity.move ( Config.spacing + Config.cardWidth, Config.cardHeight )
                )
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
      , game.deck
            |> Game.getCardsFrom game
            |> List.reverse
            |> List.map
                (\( cardId, card ) ->
                    card
                        |> viewCard [] False
                        |> Game.Entity.map (Tuple.pair (String.fromInt cardId))
                )
            |> Game.Area.mapPosition (\i _ -> Tuple.mapSecond ((+) (toFloat i * -2)))
            |> Game.Area.pileAbove ( 0, Config.cardHeight ) (viewEmptyCard "Deck")
      ]
        |> List.concat
        |> Game.Area.toHtml [ Html.Attributes.style "height" (String.fromFloat (Config.cardHeight * 2) ++ "px") ]
    , [ "Birds in your flock: " ++ (List.repeat game.flockSize "🐦" |> String.concat) |> Html.text |> Layout.el []
      , "Food:" ++ (List.repeat game.food "\u{1FAB1}" |> String.concat) |> Html.text |> Layout.el []
      ]
        |> Layout.column [ Layout.spacing 8 ]
    ]
        |> Layout.column [ Layout.spaceBetween ]
