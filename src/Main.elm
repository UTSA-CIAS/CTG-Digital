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
    , animationToggle : Bool
    , reachedAfrica : Bool
    }


type Msg
    = GotSeed Seed
    | SelectCard CardId
    | Redraw
    | ActionRequested
    | SelectDeck Deck
    | Restart
    | NewGamePlus


init : () -> ( Model, Cmd Msg )
init () =
    ( { volume = 25
      , game = Game.init
      , seed = Random.initialSeed 42
      , selectableDecks = []
      , actions = []
      , reachedAfrica = False
      , animationToggle = False
      }
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed
        ]
    )


restart : Model -> Model
restart model =
    { model
        | game = Game.init
        , selectableDecks = []
        , reachedAfrica = False
        , actions = [ Action.ChooseNewDeck ]
    }
        |> requestAction


view : Model -> Document Msg
view model =
    { title = "Waiting For Wind"
    , body =
        [ List.repeat model.game.flockSize
            (\attrs ->
                Html.text Config.birdEmoji
                    |> Layout.el ([ Html.Attributes.style "font-size" "64px" ] ++ attrs)
            )
            |> List.indexedMap (\i -> Tuple.pair ("bird_" ++ String.fromInt i))
            |> Game.Area.new ( 0, 0 )
            |> Game.Area.mapZIndex (\_ _ _ -> 2000)
            |> Game.Area.mapPosition
                (\i _ ->
                    Tuple.mapBoth
                        ((+)
                            (if model.game.flockSize <= 1 then
                                200

                             else
                                toFloat i
                                    * 300
                                    / toFloat (model.game.flockSize - 1)
                            )
                        )
                        ((+)
                            0
                        )
                )
            |> Game.Area.mapPosition
                (\i _ ->
                    if Dict.isEmpty model.game.cards then
                        Tuple.mapBoth
                            ((+)
                                (if model.animationToggle then
                                    0

                                 else
                                    25
                                )
                            )
                            ((+)
                                ((if model.animationToggle then
                                    i + 1

                                  else
                                    i
                                 )
                                    |> modBy 2
                                    |> toFloat
                                    |> (*) 25
                                )
                            )

                    else
                        Tuple.mapBoth
                            ((+)
                                (if model.animationToggle then
                                    0

                                 else
                                    -20
                                )
                            )
                            ((+)
                                0
                            )
                )
            |> Game.Area.mapRotation
                (\i _ ->
                    if Dict.isEmpty model.game.cards then
                        (+)
                            (((if model.animationToggle then
                                i + 1

                               else
                                i
                              )
                                |> modBy 2
                                |> toFloat
                                |> (*) (pi / 8)
                             )
                                - (pi / 16)
                            )

                    else
                        (+)
                            (((if model.animationToggle then
                                i + 1

                               else
                                i
                              )
                                |> modBy 2
                                |> toFloat
                                |> (*) (pi / 2)
                             )
                                + (pi
                                    * 3
                                    / 2
                                  )
                            )
                )
            |> Game.Area.toHtml [ Html.Attributes.style "width" "400px" ]
            |> Layout.el [ Layout.centerContent ]
        , View.viewGame { selectCard = SelectCard, redraw = Redraw, restart = Restart } model.game
            |> Layout.el [ Html.Attributes.style "width" "400px", Html.Attributes.style "height" "500px", Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)" ]
            |> Layout.withStack ([ Html.Attributes.style "height" "100%", Html.Attributes.style "width" "100%" ] ++ Layout.centered)
                ((if Game.gameWon model.game then
                    ( [ Html.Attributes.style "background-color" "rgba(158,228,147,0.5)" ]
                    , (if model.reachedAfrica then
                        [ Html.text "\u{1FABA}" |> Layout.el [ Html.Attributes.style "font-size" "80px", Layout.centerContent ]
                        , Html.text "You are back home. Well done!" |> Layout.el []
                        , Html.text "Restart"
                            |> View.viewButton "Restart" (Just Restart)
                            |> Layout.el [ Layout.contentCentered ]
                        ]

                       else
                        [ Html.text "🐘" |> Layout.el [ Html.Attributes.style "font-size" "80px", Layout.centerContent ]
                        , Html.text "You reached Africa. You won the game." |> Layout.el []
                        , Html.text "Travel back"
                            |> View.viewButton "Travel back" (Just NewGamePlus)
                            |> Layout.el [ Layout.contentCentered ]
                        ]
                      )
                        |> Layout.column [ Layout.spacing Config.spacing ]
                    )
                        |> Just

                  else if Game.gameOver model.game then
                    ( [ Html.Attributes.style "background-color" "rgba(100,64,62,0.5)" ]
                    , [ Html.text "💀" |> Layout.el [ Html.Attributes.style "font-size" "80px", Layout.centerContent ]
                      , Html.text "Your journey ends at Deaths doorstep" |> Layout.el []
                      , Html.text (View.viewDistanceTraveled model.game)
                      , Html.text "Restart"
                            |> View.viewButton "Restart" (Just Restart)
                            |> Layout.el [ Layout.contentCentered ]
                      ]
                        |> Layout.column
                            [ Layout.spacing Config.spacing
                            , Html.Attributes.style "background-color" "white"
                            , Html.Attributes.style "border-radius" "16px"
                            , Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)"
                            , Html.Attributes.style "padding" (String.fromFloat Config.spacing ++ "px")
                            ]
                    )
                        |> Just

                  else if Dict.isEmpty model.game.cards then
                    ( [ Html.Attributes.style "background-color" "rgba(191,219,247,1)" ]
                    , [ Html.text "Where should your flock fly to?"
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
            ( { model | seed = seed, actions = Action.ChooseNewDeck :: model.actions } |> requestAction
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
            ( requestAction { model | animationToggle = not model.animationToggle }
            , Cmd.none
            )

        Redraw ->
            ( { model | actions = Action.redraw ++ model.actions }, Cmd.none )

        SelectDeck deck ->
            ( { model | actions = Action.chooseDeck deck ++ model.actions, selectableDecks = [] } |> requestAction, Cmd.none )

        Restart ->
            ( restart model
            , Cmd.none
            )

        NewGamePlus ->
            ( { model | game = model.game |> (\g -> { g | remainingRests = Config.totalDistance }) }
                |> (\m -> { m | reachedAfrica = True })
            , Cmd.none
            )


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
