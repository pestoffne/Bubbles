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
    | MintBubble
        { shape: Circle2d.Circle2d Length.Meters Float
        , velocity: Vector2d.Vector2d Length.Meters Float }


main =
    Browser.element
    { init = init, view = view, update = update, subscriptions = subscriptions }


init () =
    ({ tick = 0, scores = 0, bubbles = [] }, Cmd.none)


view model =
    Html.div
    [ style "display" "flex", style "justify-content" "center"
    , style "align-items" "center" ]
    [ Canvas.toHtml
        ( canvasWidth, canvasHeight )
        [ style "border" "10px solid rgba(0,0,0,0.1)"
        , Mouse.onDown StartAt ]
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
                    ( remainderBy bubbleMintDelay model.tick == 0 )
                then
                    Random.generate MintBubble bubbleGenerator
                else
                    Cmd.none
            )

        StartAt mouseEvent ->
            ( burstBubbles model mouseEvent, Cmd.none )

        MintBubble mintedBubble -> ( { model
                | bubbles = mintedBubble :: model.bubbles
            }, Cmd.none )


subscriptions model =
    onAnimationFrameDelta Tick


canvasWidth = 400
canvasHeight = 512
maxBubblesCount = 7
bubbleMintDelay = 50
bubbleMinSpeed = 2.5
bubbleMaxSpeed = 5.0
bubbleMinRadius = 20
bubbleMaxRadius = 90
bubbleMinX = 30
bubbleMaxX = canvasWidth - bubbleMinX


bubbleGenerator =
    let
        mintBubble from_x to_x radius speed =
            { shape =
                Circle2d.atPoint
                ( Point2d.fromTuple Length.cssPixels ( from_x, -radius ) )
                ( Length.cssPixels radius )
            , velocity = 
                Vector2d.withLength
                ( Length.cssPixels speed )
                ( Maybe.withDefault
                    Direction2d.positiveY
                    ( Direction2d.from
                        ( Point2d.fromTuple
                            Length.cssPixels
                            ( from_x, -radius ) )
                        ( Point2d.fromTuple
                            Length.cssPixels
                            ( to_x, canvasHeight ) ) ) ) }
    in
        Random.map4
        mintBubble
        ( Random.float bubbleMinX bubbleMaxX )            -- from_x
        ( Random.float bubbleMinX bubbleMaxX )            -- to_x
        ( Random.float bubbleMinRadius bubbleMaxRadius )  -- radius
        ( Random.float bubbleMinSpeed bubbleMaxSpeed )    -- speed


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
