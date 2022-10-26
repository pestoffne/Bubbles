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


type Msg
    = Tick Float
    | Catch


width = 333
height = 512


main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> onAnimationFrameDelta Tick
    }


init () =
    ({ tick = 0, scores = 0,
        bubbles = [
            { x = 100, y = 256, radius = 30, rotation = 90 },
            { x = 200, y = 256, radius = 20, rotation = 0 }
        ]
    }, Cmd.none)


view model =
    Html.div
    [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ]
    [ Canvas.toHtml
        ( width, height )
        [ style "border" "10px solid rgba(0,0,0,0.1)", onClick Catch ]
        [ clearBox, drawBubbles model.bubbles, printScores model ]
    ]


update message model =
    case message of
        Tick _ -> ( { model | tick = model.tick + 1 }, Cmd.none )
        Catch -> ( catch model, Cmd.none )


catch model =
    { model | scores = model.scores + 1, tick = 0 }


clearBox =
    Canvas.shapes
    [ fill Color.darkGreen ]
    [ rect ( 0, 0 ) width height ]


drawOneBubble bubble =
    circle ( bubble.x, bubble.y ) bubble.radius


drawBubbles bubbles =
    Canvas.shapes
    [ fill Color.blue ]
    ( List.map drawOneBubble bubbles )


printScores model =
    Canvas.text
    [ font { size = 20, family = "serif" } , align Left, baseLine Top ]
    ( 4, 6 )
    ( "Scores = " ++ String.fromInt model.scores ++ ", Tick = " ++ String.fromInt model.tick)
