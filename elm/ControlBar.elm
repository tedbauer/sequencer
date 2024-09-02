module ControlBar exposing (..)

import Browser.Events exposing (onKeyDown)
import Debug exposing (toString)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Knob
import SoundEngineController


type alias Model =
    { bpm : Knob.Model, playing : Bool, activeStep : Int }



-- MODEL


init : Model
init =
    { bpm = Knob.init 200 150 300 1
    , playing = False
    , activeStep = 0
    }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "card" ]
        [ div
            [ class "controlbar" ]
            [ div [ class "play", onClick Play ] [ text "▶️" ]
            , div [ class "pause", onClick Pause ] [ text "⏸️" ]
            , div
                [ class "tempocontrol" ]
                [ text "Tempo", Html.map BpmMessage (Knob.view 20 model.bpm) ]
            , div [ class "maintitle" ] [ text "🎼 Pocket Symphony" ]
            ]
        ]



-- UPDATE


type Msg
    = BpmMessage Knob.Msg
    | Play
    | Pause
    | Reset
    | ProcessKeyboardEvent KeyboardEvent


processKeyboardEvent : KeyboardEvent -> Model -> ( Model, Cmd Msg )
processKeyboardEvent event model =
    case event.key of
        Maybe.Just k ->
            if k == " " then
                if model.playing then
                    ( { model | playing = False }
                    , SoundEngineController.stepEngine (SoundEngineController.encode_audio_command_message "pause")
                    )

                else
                    ( { model | playing = True }
                    , SoundEngineController.stepEngine (SoundEngineController.encode_audio_command_message "play")
                    )

            else
                ( model, Cmd.none )

        Maybe.Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BpmMessage knobMsg ->
            let
                ( newKnob, maybeValue ) =
                    Knob.update knobMsg model.bpm
            in
            ( { model | bpm = newKnob }
            , maybeValue
                |> Maybe.map round
                |> Maybe.map (SoundEngineController.stepEngine << SoundEngineController.encode_bpm_message)
                |> Maybe.withDefault Cmd.none
            )

        Play ->
            ( { model | playing = True }
            , SoundEngineController.stepEngine (SoundEngineController.encode_audio_command_message "play")
            )

        Pause ->
            ( { model | playing = False }
            , SoundEngineController.stepEngine (SoundEngineController.encode_audio_command_message "pause")
            )

        Reset ->
            ( { model | activeStep = 0 }
            , SoundEngineController.stepEngine (SoundEngineController.encode_audio_command_message "reset")
            )

        ProcessKeyboardEvent event ->
            processKeyboardEvent event model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Json.Decode.map ProcessKeyboardEvent decodeKeyboardEvent)
        , Sub.map BpmMessage
            (Knob.subscriptions model.bpm)
        ]
