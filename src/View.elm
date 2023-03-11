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
                [ Card.emoji c
                    |> Html.text
                    |> Layout.el
                        (Html.Attributes.style "font-size" "100px"
                            :: (case c of
                                    Card.Food ->
                                        [ Layout.alignAtCenter ]

                                    _ ->
                                        Layout.centered
                               )
                        )
                ]
                    |> Game.Card.default (attrs ++ [ Layout.centerContent, Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px") ])
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


viewDeckInfo : List (Attribute msg) -> List ( CardId, Card ) -> Html msg
viewDeckInfo attrs cards =
    [ Html.text "Remaining:" |> Layout.el []
    , cards
        |> List.map (\( _, card ) -> Card.emoji card)
        |> List.sort
        |> String.concat
        |> Html.text
    ]
        |> Layout.column [ Html.Attributes.style "padding" "32px 8px 8px 8px", Layout.spacing 4 ]
        |> Layout.el
            (attrs
                ++ [ Html.Attributes.style "background-color" "white"
                   , Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px")
                   , Html.Attributes.style "font-size" "0.8em"
                   , Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)"
                   , Html.Attributes.style "border-radius" " 0 0 16px 16px"
                   , Html.Attributes.style "text-align" "center"
                   ]
            )


viewGame : { selectCard : CardId -> msg, redraw : msg } -> Game -> Html msg
viewGame args game =
    [ Html.text "Waiting for Wind" |> Layout.heading1 [ Layout.centerContent ]
    , [ [ [ game.ground
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
                            |> Game.Entity.move ( Config.spacing + Config.cardWidth, 0 )
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
                |> Game.Area.pileAbove ( 0, 0 ) (viewEmptyCard "Deck")
          , [ (\attrs ->
                game.deck
                    |> Game.getCardsFrom game
                    |> viewDeckInfo attrs
              )
                |> Tuple.pair "DeckInfo"
                |> Game.Entity.new
                |> Game.Entity.move ( -1, Config.cardHeight - 32 + 4 )
            ]
          ]
            |> List.concat
            |> Game.Area.toHtml
                [ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px")
                , Html.Attributes.style "width" (String.fromFloat (Config.cardWidth * 2 + Config.spacing) ++ "px")
                ]
        , game.ground
            |> Maybe.andThen (\cardId -> Dict.get cardId game.cards |> Maybe.map (Tuple.pair cardId))
            |> Maybe.map
                (\( cardId, card ) ->
                    [ Card.name card |> Html.text |> Layout.el [ Html.Attributes.style "font-weight" "bold" ]
                    , Card.description card |> Html.text
                    ]
                        |> Layout.column []
                )
            |> Maybe.withDefault Layout.none
            |> Layout.el [ Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px") ]
        ]
            |> Layout.row [ Layout.centerContent, Layout.spacing Config.spacing ]
      , viewButton ("Redraw for 1 " ++ Config.foodEmoji) (Just args.redraw)
            |> Layout.el [ Layout.contentCentered ]
      ]
        |> Layout.column [ Layout.spacing Config.spacing ]
    , [ "Birds in your flock: " ++ (List.repeat game.flockSize Config.birdEmoji |> String.concat) |> Html.text |> Layout.el []
      , "Food:" ++ (List.repeat game.food Config.foodEmoji |> String.concat) |> Html.text |> Layout.el []
      ]
        |> Layout.column [ Layout.spacing Config.spacing ]
    ]
        |> Layout.column [ Layout.spaceBetween, Layout.fill ]


viewButton : String -> Maybe msg -> Html msg
viewButton label onClick =
    Html.text label
        |> Layout.buttonEl { onPress = onClick, label = label }
            [ Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)"
            , Html.Attributes.style "border-radius" "4px"
            ]
