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
import Task
import Time
import View
import View.Bird
import View.Overlay


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
    , birdClicked : Bool
    , musicLoaded : Bool
    }


type Msg
    = GotSeed Seed
    | SelectCard CardId
    | Redraw
    | ActionRequested
    | SelectDeck Deck
    | Restart
    | NewGamePlus
    | ToggleMute
    | PlayMusic


init : () -> ( Model, Cmd Msg )
init () =
    ( { volume = 25
      , game = Game.init
      , seed = Random.initialSeed 42
      , selectableDecks = []
      , actions = []
      , reachedAfrica = False
      , animationToggle = False
      , birdClicked = False
      , musicLoaded = False
      }
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed
        , setVolume 25
        ]
    )


restart : Model -> ( Model, Cmd Msg )
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
        [ View.Bird.toHtml { animationToggle = model.animationToggle, playMusic = PlayMusic, birdClicked = not model.musicLoaded || model.birdClicked } model.game
        , View.viewGame { selectCard = SelectCard, redraw = Redraw } model.game
            |> Layout.el (Layout.centered ++ [ Html.Attributes.style "width" "400px", Html.Attributes.style "height" "500px" ])
            |> Layout.withStack ([ Html.Attributes.style "height" "100%", Html.Attributes.style "width" "100%" ] ++ Layout.centered)
                ([ model.game
                    |> View.viewStats
                        { reachedAfrica = model.reachedAfrica
                        , isMute = model.volume == 0
                        , restart = Restart
                        , toggleMute = ToggleMute
                        , animationToggle = model.animationToggle
                        }
                    |> Tuple.pair [ Html.Attributes.style "bottom" (String.fromFloat Config.spacing ++ "px"), Html.Attributes.style "z-index" "1000" ]
                 ]
                    |> (View.Overlay.toHtml
                            { restart = Restart
                            , newGamePlus = NewGamePlus
                            , reachedAfrica = model.reachedAfrica
                            , selectDeck = SelectDeck
                            , selectableDecks = model.selectableDecks
                            , animationToggle = model.animationToggle
                            }
                            model.game
                            |> Maybe.map (::)
                            |> Maybe.withDefault identity
                       )
                )
        , View.stylesheet
        ]
    }


updateGame : (Game -> Generator ( Game, List Event )) -> Model -> ( Model, Cmd Msg )
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( ( game, events ), seed ) ->
                events
                    |> List.foldl
                        (\event ( m, c ) ->
                            applyEvent event m
                                |> Tuple.mapSecond (\head -> head :: c)
                        )
                        ( { model | game = game, seed = seed }, [] )
                    |> Tuple.mapSecond Cmd.batch
           )


applyEvent : Event -> Model -> ( Model, Cmd Msg )
applyEvent event model =
    case event of
        AddActions actions ->
            ( { model | actions = actions ++ model.actions }, Cmd.none )

        ChooseDeck decks ->
            ( { model | selectableDecks = decks }, Cmd.none )

        PlaySound sound ->
            ( model, Event.toString sound |> playSound )


requestAction : Model -> ( Model, Cmd Msg )
requestAction model =
    case model.actions of
        head :: tail ->
            { model | actions = tail } |> updateGame (Game.applyAction head)

        [] ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            { model | seed = seed, actions = Action.ChooseNewDeck :: model.actions }
                |> requestAction

        SelectCard cardId ->
            { model
                | actions =
                    (model.game.cards
                        |> Dict.get cardId
                        |> Maybe.map Action.fromCard
                        |> Maybe.withDefault []
                    )
                        ++ model.actions
            }
                |> requestAction

        ActionRequested ->
            requestAction { model | animationToggle = not model.animationToggle }

        Redraw ->
            { model | actions = Action.redraw ++ model.actions } |> requestAction

        SelectDeck deck ->
            { model | actions = Action.chooseDeck deck ++ model.actions, selectableDecks = [], musicLoaded = True }
                |> requestAction
                |> Tuple.mapSecond (\c -> Cmd.batch [ c, Event.sounds |> List.map loadSound |> Cmd.batch ])

        Restart ->
            restart model

        NewGamePlus ->
            ( { model | game = model.game |> (\g -> { g | remainingRests = Config.totalDistance }) }
                |> (\m -> { m | reachedAfrica = True })
            , Cmd.none
            )

        ToggleMute ->
            if model.volume == 0 then
                ( { model
                    | volume = 25
                  }
                , setVolume 25
                )

            else
                ( { model
                    | volume = 0
                  }
                , setVolume 0
                )

        PlayMusic ->
            ( { model | birdClicked = model.musicLoaded, musicLoaded = True }
            , if not model.musicLoaded then
                Event.sounds
                    |> List.map loadSound
                    |> (::) (playSound (Event.toString Event.Singing))
                    |> Cmd.batch

              else
                playSound (Event.toString Event.Singing)
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
