port module Main exposing (..)

import Action exposing (Action)
import Browser exposing (Document)
import Card exposing (CardId)
import Config
import Deck exposing (Deck)
import Dict
import Event exposing (Event(..))
import Game exposing (Game)
import Game.Area
import Game.Entity
import Html
import Html.Attributes
import Layout
import Random exposing (Generator, Seed)
import Time
import View


port loadSound : ( String, String ) -> Cmd msg


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type alias Model =
    { game : Game
    , volume : Int
    , seed : Seed
    , selectableDecks : List Deck
    , actions : List Action
    }


type Msg
    = GotSeed Seed
    | SelectCard CardId
    | Redraw
    | ActionRequested
    | SelectDeck Deck


init : () -> ( Model, Cmd Msg )
init () =
    ( { volume = 25
      , game = Game.init
      , seed = Random.initialSeed 42
      , selectableDecks = [ Deck.Beach ]
      , actions = []
      }
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed
        ]
    )


view : Model -> Document Msg
view model =
    { title = "Waiting 4 Wind"
    , body =
        [ View.viewGame { selectCard = SelectCard, redraw = Redraw } model.game
            |> Layout.el [ Html.Attributes.style "width" "400px", Html.Attributes.style "height" "500px", Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)" ]
            |> Layout.withStack ([ Html.Attributes.style "height" "100%", Html.Attributes.style "width" "100%" ] ++ Layout.centered)
                ((if Game.gameWon model.game then
                    ( [ Html.Attributes.style "background-color" "rgba(158,228,147,0.5)" ]
                    , Html.text "You have reached Africa. Your Journey is over"
                        |> Layout.el []
                    )
                        |> Just

                  else if Game.gameOver model.game then
                    ( [ Html.Attributes.style "background-color" "rgba(100,64,62,0.5)" ]
                    , Html.text "You Journey has ended as you reach Deaths doorstep"
                        |> Layout.el []
                    )
                        |> Just

                  else if Dict.isEmpty model.game.cards then
                    ( [ Html.Attributes.style "background-color" "rgba(191,219,247,1)" ]
                    , [ Html.text "Where should your flock land?"
                            |> Layout.heading2 [ Html.Attributes.style "padding" (String.fromFloat (Config.spacing + 2) ++ "px 0") ]
                      , model.selectableDecks
                            |> List.map
                                (\deck ->
                                    View.viewCardBack
                                        (Layout.asButton
                                            { onPress = Just (SelectDeck deck), label = "Select " ++ Deck.name deck ++ "Deck" }
                                        )
                                        deck
                                        |> View.viewDeck (Deck.cards deck)
                                        |> Game.Entity.toHtml []
                                )
                            |> Layout.row [ Layout.spacing Config.spacing, Layout.contentCentered ]
                      , View.viewStats model.game
                      ]
                        |> Layout.column [ Layout.spacing Config.spacing ]
                    )
                        |> Just

                  else
                    Nothing
                 )
                    |> Maybe.map
                        (\( attrs, content ) ->
                            [ ( [ Html.Attributes.style "width" "100%"
                                , Html.Attributes.style "height" "100%"
                                ]
                              , content
                                    |> Layout.el
                                        (Layout.centered
                                            ++ [ Html.Attributes.style "width" "100%"
                                               , Html.Attributes.style "height" "100%"
                                               , Html.Attributes.style "backdrop-filter" "blur(4px)"
                                               , Html.Attributes.style "z-index" "100"
                                               , Html.Attributes.style "position" "relative"
                                               ]
                                            ++ attrs
                                        )
                              )
                            ]
                        )
                    |> Maybe.withDefault []
                )
        , Html.node "style"
            []
            [ """
@font-face {
    font-family: "NotoEmoji";
    src: url("assets/NotoEmoji.ttf");
  }
@font-face {
    font-family: "NotoEmojiColor";
    src: url("assets/NotoEmojiColor.ttf");
  }
:root {
    --back-color1: #e5e5f7;
    --back-color2: #444cf7;
}

:root,body {
    height:100%;
    background-color:#f4f3ee;
    font-family: serif,"NotoEmojiColor";
}

button {
    font-family: serif,"NotoEmojiColor";
}

button:hover {
    filter: brightness(0.95)
}

button:focus {
    filter: brightness(0.90)
}

button:active {
    filter: brightness(0.7)
}
"""
                |> Html.text
            ]
        ]
    }


updateGame : (Game -> Generator ( Game, List Event )) -> Model -> Model
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( ( game, events ), seed ) ->
                events
                    |> List.foldl applyEvent
                        { model | game = game, seed = seed }
           )


applyEvent : Event -> Model -> Model
applyEvent event model =
    case event of
        AddActions actions ->
            { model | actions = actions ++ model.actions }

        ChooseDeck decks ->
            { model | selectableDecks = decks }


requestAction : Model -> Model
requestAction model =
    case model.actions of
        head :: tail ->
            { model | actions = tail } |> updateGame (Game.applyAction head)

        [] ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            ( { model | seed = seed, actions = [ Action.Shuffle, Action.DrawCard ] ++ model.actions }
            , Cmd.none
            )

        SelectCard cardId ->
            ( { model
                | actions =
                    (model.game.cards
                        |> Dict.get cardId
                        |> Maybe.map Action.fromCard
                        |> Maybe.withDefault []
                    )
                        ++ model.actions
              }
                |> requestAction
            , Cmd.none
            )

        ActionRequested ->
            ( requestAction model
            , Cmd.none
            )

        Redraw ->
            ( { model | actions = Action.redraw ++ model.actions }, Cmd.none )

        SelectDeck deck ->
            ( { model | actions = Action.chooseDeck deck ++ model.actions } |> requestAction, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 (\_ -> ActionRequested)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
