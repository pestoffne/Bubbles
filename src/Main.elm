module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (..)
import Circle2d
import Color
import Direction2d
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Length
import Point2d
import Vector2d
import Random


type Msg
    = Tick Float
    | StartAt Mouse.Event
    --| MoveAt Mouse.Event
    --| EndAt Mouse.Event
    -- from_x, to_x, radius, velocity
    --| MintBubble ( Float, Float, Float, Float )
    | MintBubble Float


canvasWidth = 400
canvasHeight = 512
maxBubblesCount = 7
bubbleGenerationRate = 0.05
bubbleMinSpeed = 2.5
bubbleMaxSpeed = 5.0
bubbleMinRadius = 20
bubbleMaxRadius = 90
bubbleMinX = 30
bubbleMaxX = canvasWidth - bubbleMinX
bubbleGenerator = Random.float bubbleMinX bubbleMaxX


main =
    Browser.element
    { init = init, view = view, update = update, subscriptions = subscriptions }


init () =
    ({ tick = 0, scores = 0, bubbles = [
        -- first bubble
        { shape =
            Circle2d.atPoint
            ( Point2d.fromTuple Length.cssPixels ( 100, 256 ) )
            ( Length.cssPixels 100 )
        , velocity =
            Vector2d.withLength
            ( Length.cssPixels 2.5 )
            Direction2d.positiveX },
        -- second bubble
        { shape =
            Circle2d.atPoint
            ( Point2d.fromTuple Length.cssPixels ( 300, 256 ) )
            ( Length.cssPixels 30 )
        , velocity =
            Vector2d.withLength
            ( Length.cssPixels 5 )
            Direction2d.positiveY }
    ]}, Cmd.none)


view model =
    Html.div
    [ style "display" "flex", style "justify-content" "center"
    , style "align-items" "center" ]
    [ Canvas.toHtml
        ( canvasWidth, canvasHeight )
        [ style "border" "10px solid rgba(0,0,0,0.1)"
        , Mouse.onDown StartAt ]
        --, Mouse.onMove MoveAt
        --, Mouse.onUp EndAt
        [ clearCanvas
        , renderBubbles model.bubbles
        , renderScores model ]
    ]


update message model =
    case message of
        Tick _ -> ( { model
                | tick = model.tick + 1
                , bubbles = moveBubbles model.bubbles
            },
                if 
                    ( List.length model.bubbles < maxBubblesCount ) &&
                    ( remainderBy 100 model.tick == 0 )
                then
                    Random.generate MintBubble bubbleGenerator
                else
                    Cmd.none
            )

        StartAt mouseEvent ->
            ( burstBubbles model mouseEvent, Cmd.none )

        MintBubble x -> ( { model
                | bubbles = mintBubble model.bubbles x
            }, Cmd.none )

        --MoveAt _ -> ( model, Cmd.none )
        --EndAt _ -> ( model, Cmd.none )


subscriptions model =
    onAnimationFrameDelta Tick


burstBubbles model mouseEvent =
    let
        isTouched touchPos bubble =
            Circle2d.contains
            ( Point2d.fromTuple Length.cssPixels touchPos )
            bubble.shape

        isNotTouched touchPos bubble =
            not ( isTouched touchPos bubble )

        touchedBubbles =
            List.filter
            ( isTouched mouseEvent.offsetPos )
            model.bubbles

        untouchedBubbles =
            List.filter
            ( isNotTouched mouseEvent.offsetPos )
            model.bubbles
    in
        { model
        | scores = model.scores + ( List.length touchedBubbles )
        , bubbles = untouchedBubbles }


moveBubbles bubbles = 
    let
        moveBubble bubble =
            { bubble | shape =
                Circle2d.translateBy bubble.velocity bubble.shape }
    in
        List.map moveBubble bubbles


mintBubble bubbles x =
    { shape =
        Circle2d.atPoint
        ( Point2d.fromTuple Length.cssPixels ( x, 0 ) )  -- center
        ( Length.cssPixels 40 )  -- radius
    , velocity =
        Vector2d.withLength
        ( Length.cssPixels 2.5 )  -- speed
        Direction2d.positiveY
    } :: bubbles


clearCanvas =
    Canvas.shapes
    [ fill Color.darkGreen ]
    [ Canvas.rect ( 0, 0 ) canvasWidth canvasHeight ]


renderBubbles bubbles =
    let
        shapeBubble bubble =
            Canvas.circle
            ( Point2d.toTuple Length.inCssPixels (
              Circle2d.centerPoint bubble.shape ) )
            ( Length.inCssPixels (Circle2d.radius bubble.shape) )
    in
        Canvas.shapes
        [ fill Color.blue ]
        ( List.map shapeBubble bubbles )


renderScores model =
    Canvas.text
    [ font { size = 24, family = "serif" } , align Left, baseLine Top ]
    ( 4, 6 )  -- padding
    ( "Scores = " ++ String.fromInt model.scores ++ ", Tick = "
    ++ String.fromInt model.tick )
