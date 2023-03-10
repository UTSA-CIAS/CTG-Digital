port module Main exposing (..)

import Browser exposing (Document)
import Html
import Random exposing (Seed)


port loadSound : ( String, String ) -> Cmd msg


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type alias Model =
    { volume : Int
    , seed : Seed
    }


type Msg
    = GotSeed Seed


init : () -> ( Model, Cmd Msg )
init () =
    ( { volume = 25, seed = Random.initialSeed 42 }
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed
        ]
    )


view : Model -> Document Msg
view _ =
    { title = "Game Jam"
    , body =
        [ Html.text "you need to click a button to activate sound"
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
:root,body {
    height:100%;background-color:#f4f3ee;
    font-family: serif,"NotoEmojiColor";"""
                |> Html.text
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
