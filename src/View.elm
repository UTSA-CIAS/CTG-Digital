module View exposing (..)

import Card exposing (Card, CardId)
import Config
import Deck exposing (Deck, primaryColor, secondaryColor)
import Dict exposing (Dict)
import Game exposing (Game)
import Game.Area
import Game.Card
import Game.Entity exposing (Entity)
import Html exposing (Attribute, Html, label)
import Html.Attributes
import Layout


viewCard : List (Html.Attribute msg) -> { faceUp : Bool, card : Card, deck : Deck } -> Game.Entity.Entity (List (Html.Attribute msg) -> Html msg)
viewCard attributes args =
    Game.Entity.flippable
        ([ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px")
         , Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px")
         ]
            ++ attributes
        )
        { front =
            (\attrs ->
                [ Card.emoji args.card
                    |> Html.text
                    |> Layout.el
                        (Html.Attributes.style "font-size" "100px"
                            :: (case args.card of
                                    Card.Food ->
                                        [ Layout.alignAtCenter ]

                                    Card.Friend ->
                                        [ Layout.alignAtCenter ]

                                    _ ->
                                        Layout.centered
                               )
                        )
                ]
                    |> Game.Card.default (attrs ++ [ Layout.centerContent, Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px") ])
            )
                |> Game.Entity.new
        , back = viewCardBack [] args.deck
        , faceUp = args.faceUp
        }


viewCardBack : List (Attribute msg) -> Deck -> Game.Entity.Entity (List (Html.Attribute msg) -> Html msg)
viewCardBack a deck =
    let
        primaryColor =
            Deck.primaryColor deck

        secondaryColor =
            Deck.secondaryColor deck
    in
    (\attrs ->
        Deck.emoji deck
            |> Html.text
            |> Layout.el
                [ Html.Attributes.style "font-size" "40px"
                , Html.Attributes.style "background-color" secondaryColor
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "border-radius" "100%"
                ]
            |> Game.Card.back
                ([ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px")
                 , Html.Attributes.style "background-color" primaryColor
                 , Html.Attributes.style "background-image" ("linear-gradient(135deg, " ++ secondaryColor ++ " 25%, transparent 25%), linear-gradient(225deg," ++ secondaryColor ++ " 25%, transparent 25%), linear-gradient(45deg, " ++ secondaryColor ++ " 25%, transparent 25%), linear-gradient(315deg, " ++ secondaryColor ++ " 25%, " ++ primaryColor ++ " 25%)")
                 , Html.Attributes.style "background-position" "40px 0, 40px 0, 0 0, 0 0"
                 , Html.Attributes.style "background-size" "40px 40px"
                 , Html.Attributes.style "background-repeat" "repeat"
                 ]
                    ++ a
                    ++ attrs
                )
    )
        |> Game.Entity.new


viewEmptyCard : String -> ( String, List (Attribute msg) -> Html msg )
viewEmptyCard name =
    ( name
    , \attrs ->
        Game.Card.empty
            (attrs ++ [ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px") ])
            name
    )


viewDeck : List Card -> Entity (List (Attribute msg) -> Html msg) -> Entity (List (Attribute msg) -> Html msg)
viewDeck cards back =
    List.repeat (List.length cards) back
        |> Game.Area.mapPosition (\i _ -> Tuple.mapSecond ((+) (toFloat i * -2)))
        |> (::)
            ((\attrs ->
                cards
                    |> viewDeckInfo attrs "Content:"
             )
                |> Game.Entity.new
                |> Game.Entity.move ( 0, Config.cardHeight - 32 + 4 )
            )
        |> Game.Entity.pileAbove
            (Layout.el
                [ Html.Attributes.style "height" (String.fromFloat (Config.cardHeight + 100) ++ "px")
                , Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px")
                ]
                Layout.none
            )


group : List String -> Dict String Int
group =
    List.foldl
        (\key ->
            Dict.update key
                (\maybe ->
                    maybe
                        |> Maybe.map ((+) 1)
                        |> Maybe.withDefault 1
                        |> Just
                )
        )
        Dict.empty


viewDeckInfo : List (Attribute msg) -> String -> List Card -> Html msg
viewDeckInfo attrs label cards =
    [ Html.text label |> Layout.el [ Html.Attributes.style "font-size" "0.8em", Html.Attributes.style "color" "rgba(0,0,0,0.5)" ]
    , cards
        |> List.map Card.emoji
        |> group
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.map
            (\( string, amount ) ->
                List.repeat amount string
                    |> String.concat
                    |> Html.text
                    |> Layout.el []
            )
        |> Layout.column [ Html.Attributes.style "font-size" "0.8em" ]
    ]
        |> Layout.column [ Html.Attributes.style "padding" "32px 8px 8px 8px", Layout.spacing 4 ]
        |> Layout.el
            [ Html.Attributes.style "background-color" "white"
            , Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)"
            , Html.Attributes.style "border-radius" " 0 0 16px 16px"
            , Html.Attributes.style "text-align" "center"
            ]
        |> Layout.el
            (attrs
                ++ [ Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px")
                   , Layout.centerContent
                   ]
            )


viewGame : { selectCard : CardId -> msg, redraw : msg, restart : msg, toggleMute : msg, isMute : Bool } -> Game -> Html msg
viewGame args game =
    [ [ Html.text "Waiting for Wind" |> Layout.heading1 [ Layout.contentCentered ]
      ]
        |> Layout.column []
    , [ [ [ game.ground
                |> Maybe.andThen (\cardId -> Dict.get cardId game.cards |> Maybe.map (Tuple.pair cardId))
                |> Maybe.map
                    (\( cardId, card ) ->
                        viewCard
                            (Layout.asButton
                                { onPress = Just (args.selectCard cardId)
                                , label = "Select " ++ Card.name card
                                }
                            )
                            { faceUp = True
                            , card = card
                            , deck = game.deckType
                            }
                            |> Game.Entity.map (Tuple.pair (String.fromInt cardId))
                            |> Game.Entity.mapZIndex ((+) 50)
                            |> Game.Entity.move ( Config.spacing + Config.cardWidth, 0 )
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
          , game.deck
                |> Game.getCardsFrom game
                |> List.reverse
                |> List.map
                    (\( cardId, card ) ->
                        viewCard [] { faceUp = False, card = card, deck = game.deckType }
                            |> Game.Entity.map (Tuple.pair (String.fromInt cardId))
                    )
                |> Game.Area.mapPosition (\i _ -> Tuple.mapSecond ((+) (toFloat i * -2)))
                |> Game.Area.pileAbove ( 0, 0 ) (viewEmptyCard "Deck")
          , [ (\attrs ->
                game.deck
                    |> Game.getCardsFrom game
                    |> List.map Tuple.second
                    |> viewDeckInfo attrs "contains:"
              )
                |> Tuple.pair "DeckInfo"
                |> Game.Entity.new
                |> Game.Entity.move ( 0, Config.cardHeight - 32 + 4 )
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
      , [ Html.text "Click the card" |> Layout.el [ Layout.contentCentered ]
        , Html.text "or" |> Layout.el [ Layout.contentCentered ]
        , [ "Redraw" |> Html.text |> Layout.el []
          , "for 1" ++ Config.foodEmoji |> Html.text |> Layout.el []
          ]
            |> Layout.column []
            |> viewButton
                ("Redraw for 1 " ++ Config.foodEmoji)
                (if game.food > 0 then
                    Just args.redraw

                 else
                    Nothing
                )
            |> Layout.el [ Layout.contentCentered ]
        ]
            |> Layout.column [ Html.Attributes.style "width" (String.fromFloat Config.cardWidth ++ "px") ]
            |> Layout.el [ Layout.contentCentered ]
      ]
        |> Layout.column [ Layout.spacing Config.spacing ]
    , [ viewStats game
      , [ Html.text "Restart"
            |> viewButton "Restart" (Just args.restart)
            |> Layout.el [ Layout.contentCentered, Layout.alignAtEnd ]
        , (if args.isMute then
            "🔊 Unmute"

           else
            "🔇 Mute"
          )
            |> (\label ->
                    label
                        |> Html.text
                        |> viewButton label (Just args.toggleMute)
               )
        ]
            |> Layout.column [ Layout.spacing Config.spacing ]
      ]
        |> Layout.row [ Layout.spaceBetween ]
    ]
        |> Layout.column [ Layout.spaceBetween, Layout.fill ]


viewStats : Game -> Html msg
viewStats game =
    [ "Food: " ++ (List.repeat game.food Config.foodEmoji |> String.concat) |> Html.text |> Layout.el []
    , viewDistanceTraveled game
        |> Html.text
        |> Layout.el []
    ]
        |> Layout.column [ Layout.spacing Config.spacing ]


viewDistanceTraveled : Game -> String
viewDistanceTraveled game =
    "Distance Traveled: "
        ++ (if game.remainingRests == Config.totalDistance then
                "0"

            else
                String.fromInt (Config.totalDistance - game.remainingRests) ++ ".000"
           )
        ++ " km / "
        ++ String.fromInt Config.totalDistance
        ++ ".000 km"


viewButton : String -> Maybe msg -> Html msg -> Html msg
viewButton label onClick content =
    content
        |> Layout.buttonEl { onPress = onClick, label = label }
            [ Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)"
            , Html.Attributes.style "border-radius" "4px"
            , Html.Attributes.style "padding" "4px 8px"
            , Html.Attributes.style "background-color" "#9EE493"
            , Html.Attributes.disabled (onClick == Nothing)
            ]
