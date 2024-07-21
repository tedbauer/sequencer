port module Main exposing (..)

import Browser
import ControlBar
import DrumMachine
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseDown)
import Lfo
import Platform.Cmd as Cmd
import Sequencer


type alias Model =
    { controlBar : ControlBar.Model
    , sequencer : Sequencer.Model
    , drumMachine : DrumMachine.Model
    , lfo : Lfo.Model
    , lfoCmd : Cmd Msg
    }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( lfoModel, lfoCmd ) =
            Lfo.init
    in
    ( { controlBar = ControlBar.init
      , sequencer = Sequencer.init
      , drumMachine = DrumMachine.init
      , lfo = lfoModel
      , lfoCmd = Cmd.map LfoMsg lfoCmd
      }
    , Cmd.batch [ Cmd.map LfoMsg lfoCmd ]
    )



-- VIEW


generateWavePoints : Int -> Float -> Float -> String
generateWavePoints width amplitude frequency =
    List.range 0 width
        |> List.map
            (\x ->
                let
                    y =
                        amplitude * sin (frequency * toFloat x * pi / 180)
                in
                String.fromInt x ++ "," ++ String.fromFloat (100 - y)
            )
        |> String.join " "


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "cardrows" ]
            [ div [ class "cardrow" ]
                [ Html.map ControlBarMsg (ControlBar.view model.controlBar)
                ]
            , div [ class "cardrow" ]
                [ Html.map SequencerMsg (Sequencer.view model.sequencer)
                , Html.map DrumMachineMsg (DrumMachine.view model.drumMachine)
                ]
            , div [ class "cardrow" ]
                [ Html.map LfoMsg (Lfo.view model.lfo)
                , div [ class "card" ]
                    [ div
                        [ class "cardtitle" ]
                        [ text "🔊 Delay" ]
                    ]
                ]
            ]
        ]



-- UPDATE


type Msg
    = SetCurrentStep Int
    | ControlBarMsg ControlBar.Msg
    | SequencerMsg Sequencer.Msg
    | DrumMachineMsg DrumMachine.Msg
    | LfoMsg Lfo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCurrentStep step ->
            let
                ( updatedDrumMachine, _ ) =
                    DrumMachine.update (DrumMachine.SetCurrentStep step) model.drumMachine

                ( updatedSequencer, _ ) =
                    Sequencer.update (Sequencer.SetCurrentStep step) model.sequencer
            in
            ( { model | drumMachine = updatedDrumMachine, sequencer = updatedSequencer }, Cmd.none )

        ControlBarMsg controlBarMsg ->
            let
                ( subModel, subCmd ) =
                    ControlBar.update controlBarMsg model.controlBar
            in
            ( { model | controlBar = subModel }, Cmd.map ControlBarMsg subCmd )

        DrumMachineMsg drumMachineMsg ->
            let
                ( subModel, subCmd ) =
                    DrumMachine.update drumMachineMsg model.drumMachine
            in
            ( { model | drumMachine = subModel }, Cmd.map DrumMachineMsg subCmd )

        SequencerMsg sequencerMsg ->
            let
                ( subModel, subCmd ) =
                    Sequencer.update sequencerMsg model.sequencer
            in
            ( { model | sequencer = subModel }, Cmd.map SequencerMsg subCmd )

        LfoMsg lfoMsg ->
            let
                ( updatedLfo, lfoCmd ) =
                    Lfo.update lfoMsg model.lfo
            in
            ( { model | lfo = updatedLfo, lfoCmd = Cmd.map LfoMsg lfoCmd }
            , Cmd.map LfoMsg lfoCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveCurrentStepUpdate
            SetCurrentStep
        , Sub.map ControlBarMsg ControlBar.subscriptions
        ]



-- PORTS


port receiveCurrentStepUpdate : (Int -> msg) -> Sub msg



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }
