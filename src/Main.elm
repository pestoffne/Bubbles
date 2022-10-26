module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    { count : Float }


type Msg
    = Tick Float
    | Catch


width = 512
height = 512


main =
    Browser.element
        { init = \() -> ( { tick = 0, scores = 0, bubbles = [] }, Cmd.none )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Tick _ ->
                        ( { model | tick = model.tick + 1 }, Cmd.none )
                    Catch ->
                        ( catch model, Cmd.none )
        , subscriptions = \model -> onAnimationFrameDelta Tick
        }


view model =
    Html.div
    [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ]
    [ Canvas.toHtml
        ( width, height )
        [ style "border" "10px solid rgba(0,0,0,0.1)", onClick Catch ]
        [ clearBox, printScores model ]
    ]


catch model =
    { model
        | scores = model.scores + 1
        , tick = 0
    }


clearBox =
    Canvas.shapes
    [ fill Color.darkGreen ]
    [ rect ( 0, 0 ) width height ]


printScores model =
    Canvas.text
    [ font { size = 32, family = "serif" } , align Left ]
    ( 4, 50 )
    ( "Scores = " ++ String.fromInt model.scores ++ ", Tick = " ++ String.fromInt model.tick)
