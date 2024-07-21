module Knob exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style, id)
import Html.Events exposing (on)
import Json.Decode as Decode
import Browser.Events
import Task
import Browser.Dom

type alias Model =
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , label : String
    , isDragging : Bool
    , lastMousePosition : { x : Float, y : Float }
    , centerX : Float
    , centerY : Float
    }

type Msg
    = SetValue Float
    | StartDrag Float Float
    | ContinueDrag Float Float
    | EndDrag
    | SetCenter Float Float

init : String -> Float -> Float -> Float -> Float -> ( Model, Cmd Msg )
init label min max step initialValue =
    ( { min = min
      , max = max
      , step = step
      , value = initialValue
      , label = label
      , isDragging = False
      , lastMousePosition = { x = 0, y = 0 }
      , centerX = 30
      , centerY = 30
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetValue newValue ->
            let
                clampedValue =
                    clamp model.min model.max newValue
            in
            ( { model | value = clampedValue }, Cmd.none )

        StartDrag x y ->
            ( { model | isDragging = True, lastMousePosition = { x = x, y = y } }, Cmd.none )

        ContinueDrag x y ->
            if model.isDragging then
                let
                    dx = x - model.lastMousePosition.x
                    dy = y - model.lastMousePosition.y
                    sensitivity = 0.01  -- Increased sensitivity
                    delta = (dx - dy) * sensitivity
                    newValue = model.value + delta
                    clampedValue = clamp model.min model.max (roundToStep newValue model.step)
                in
                ( { model | value = clampedValue, lastMousePosition = { x = x, y = y } }, Cmd.none )
            else
                ( model, Cmd.none )

        EndDrag ->
            ( { model | isDragging = False }, Cmd.none )

        SetCenter x y ->
            ( { model | centerX = x, centerY = y }, Cmd.none )

view : Model -> Html Msg
view model =
    let
        rotation =
            valueToRotation model
    in
    div
        [ class "knob-container"
        , id "lfo-frequency-knob"
        ]
        [ div
            [ class "knob"
            , class (if model.isDragging then "dragging" else "")
            , style "transform" ("rotate(" ++ String.fromFloat rotation ++ "deg)")
            , on "mousedown" (Decode.map2 StartDrag (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float))
            ]
            []
        , div [ class "knob-label" ] [ text (model.label ++ ": " ++ String.fromFloat (roundToDecimal 2 model.value)) ]
        ]

valueToRotation : Model -> Float
valueToRotation model =
    (model.value - model.min) / (model.max - model.min) * 300 - 150

roundToDecimal : Int -> Float -> Float
roundToDecimal places value =
    let
        factor =
            toFloat (10 ^ places)
    in
    toFloat (round (value * factor)) / factor

roundToStep : Float -> Float -> Float
roundToStep value step =
    toFloat (round (value / step)) * step

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> Cmd.none)
        , Browser.Events.onAnimationFrame (\_ ->
            Task.attempt
                (\result ->
                    case result of
                        Ok { element } ->
                            SetCenter (element.x + element.width / 2) (element.y + element.height / 2)
                        Err _ ->
                            SetCenter 30 30
                )
                (Browser.Dom.getElement "lfo-frequency-knob")
          )
        , if model.isDragging then
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map2 ContinueDrag (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float))
                , Browser.Events.onMouseUp (Decode.succeed EndDrag)
                ]
          else
            Sub.none
        ]
