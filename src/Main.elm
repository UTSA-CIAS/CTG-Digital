port module Main exposing (..)

import Action exposing (Action)
import Browser exposing (Document)
import Card exposing (CardId)
import Game exposing (Game)
import Game.Area
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
    , actions : List Action
    }


type Msg
    = GotSeed Seed
    | SelectCard CardId
    | Redraw
    | ActionRequested


init : () -> ( Model, Cmd Msg )
init () =
    ( { volume = 25
      , game = Game.init
      , seed = Random.initialSeed 42
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
            |> Layout.el [ Html.Attributes.style "width" "400px", Html.Attributes.style "height" "400px", Html.Attributes.style "border" "1px solid rgba(0,0,0,0.2)" ]
            |> Layout.withStack []
                (if Game.gameOver model.game then
                    [ ( []
                      , Html.text "You Dead"
                            |> Layout.el []
                            |> Layout.el
                                (Layout.centered
                                    ++ [ Html.Attributes.style "width" "400px"
                                       , Html.Attributes.style "height" "400px"
                                       , Html.Attributes.style "backdrop-filter" "blur(4px)"
                                       , Html.Attributes.style "z-index" "100"
                                       , Html.Attributes.style "position" "relative"
                                       ]
                                )
                      )
                    ]

                 else
                    []
                )
            |> Layout.el ([ Html.Attributes.style "height" "100%" ] ++ Layout.centered)
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

.cardBack {
    background-color: var(--back-color1);
background-image:  linear-gradient(135deg, var(--back-color2) 25%, transparent 25%), linear-gradient(225deg, var(--back-color2) 25%, transparent 25%), linear-gradient(45deg, var(--back-color2) 25%, transparent 25%), linear-gradient(315deg, var(--back-color2) 25%, var(--back-color1) 25%);
background-position:  40px 0, 40px 0, 0 0, 0 0;
background-size: 40px 40px;
background-repeat: repeat;
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


updateGame : (Game -> Generator Game) -> Model -> Model
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( game, seed ) ->
                { model | game = game, seed = seed }
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            ( { model | seed = seed, actions = [ Action.Shuffle, Action.DrawCard ] ++ model.actions }
            , Cmd.none
            )

        SelectCard cardId ->
            ( model |> updateGame (Game.playCard cardId)
            , Cmd.none
            )

        ActionRequested ->
            ( case model.actions of
                head :: tail ->
                    { model | actions = tail } |> updateGame (Game.applyAction head)

                [] ->
                    model
            , Cmd.none
            )

        Redraw ->
            ( { model | actions = Action.redraw ++ model.actions }, Cmd.none )


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
