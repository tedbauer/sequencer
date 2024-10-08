port module Sequencer exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Debug exposing (toString)
import DrumMachine exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseDown)
import SoundEngineController
import String exposing (toInt)
import WebAudio exposing (oscillator)


type alias Model =
    { melody : Array Chord, activeChord : Int, oscillator : Oscillator, bpm : Int, octave : Int }


type NoteDefinition
    = NotPopulated
    | Populated Note


type alias Chord =
    Array NoteDefinition


type Wave
    = Sine
    | Square
    | Triangle


type alias Oscillator =
    { wave : Wave }


type OscillatorUpdate
    = NextWave
    | PrevWave


type Note
    = C
    | D
    | E
    | F
    | G
    | A
    | B
    | LowG



-- MODEL


init : Model
init =
    { melody = Array.repeat 8 (Array.repeat 8 NotPopulated)
    , activeChord = 0
    , oscillator = { wave = Sine }
    , bpm = 200
    , octave = 0
    }



-- VIEW


isNotePopulated : Model -> Int -> Int -> Bool
isNotePopulated model rowNumber columnNumber =
    case Array.get columnNumber model.melody of
        Maybe.Just chord ->
            case Array.get rowNumber chord of
                Maybe.Just (Populated _) ->
                    True

                Maybe.Just NotPopulated ->
                    False

                Maybe.Nothing ->
                    False

        Maybe.Nothing ->
            False


createNote : Int -> Int -> Model -> Html Msg
createNote rowNumber columnNumber model =
    div
        [ onMouseDown
            (SetNote
                ( rowNumber
                , columnNumber
                , if isNotePopulated model rowNumber columnNumber then
                    NotPopulated

                  else
                    case rowNumber of
                        0 ->
                            Populated G

                        1 ->
                            Populated F

                        2 ->
                            Populated E

                        3 ->
                            Populated D

                        4 ->
                            Populated C

                        5 ->
                            Populated B

                        6 ->
                            Populated A

                        7 ->
                            Populated LowG

                        _ ->
                            Populated A
                )
            )
        , class "note"
        , attribute "data-enabled"
            (if isNotePopulated model rowNumber columnNumber then
                "true"

             else
                "false"
            )
        ]
        []


createChord : Int -> Int -> Int -> Model -> List (Html Msg)
createChord currNote maxNotes columnNumber model =
    if maxNotes == currNote then
        []

    else
        createNote currNote columnNumber model :: createChord (currNote + 1) maxNotes columnNumber model


chordView : Int -> Model -> Html Msg
chordView columnNumber model =
    div
        [ class "chord"
        , attribute "data-on"
            (if model.activeChord == columnNumber then
                "true"

             else
                "false"
            )
        ]
        (createChord 0 8 columnNumber model)


stringOfWave : Wave -> String
stringOfWave wave =
    case wave of
        Sine ->
            "Sine"

        Square ->
            "Square"

        Triangle ->
            "Triangle"


determineActiveX : Model -> String
determineActiveX model =
    toString (5 + model.activeChord * 37) ++ "px"


sequencerCard : Model -> Html Msg
sequencerCard model =
    div [ class "card" ]
        [ div [ class "cardtitle" ] [ text "🧮 Sequencer" ]
        , div [ class "playbar" ]
            [ div [ class "activenote", style "left" (determineActiveX model) ] [ text "⬇️" ] ]
        , div
            [ class "sequencer" ]
            [ div []
                [ chordView 0 model
                , chordView 1 model
                , chordView 2 model
                , chordView 3 model
                , chordView 4 model
                , chordView 5 model
                , chordView 6 model
                , chordView 7 model
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    sequencerCard model



-- UPDATE


type Msg
    = SetNote ( Int, Int, NoteDefinition )
    | SetCurrentStep Int
    | SetOctave Int
    | UpdateOscillator OscillatorUpdate


prevWave : Wave -> Wave
prevWave wave =
    case wave of
        Square ->
            Sine

        Triangle ->
            Square

        Sine ->
            Triangle


nextWave : Wave -> Wave
nextWave wave =
    case wave of
        Sine ->
            Square

        Square ->
            Triangle

        Triangle ->
            Sine


isNote : NoteDefinition -> Bool
isNote note =
    case note of
        Populated _ ->
            True

        NotPopulated ->
            False


chordToFrequencies : List NoteDefinition -> List Float
chordToFrequencies noteDefinitions =
    List.filter isNote noteDefinitions
        |> List.map
            (\note ->
                case note of
                    Populated C ->
                        261.63

                    Populated D ->
                        293.66

                    Populated E ->
                        329.63

                    Populated F ->
                        349.23

                    Populated G ->
                        392.0

                    Populated A ->
                        220.0

                    Populated B ->
                        246.94

                    Populated LowG ->
                        196.0

                    NotPopulated ->
                        -1.0
            )


melodyFrequencies : Model -> List (List Float)
melodyFrequencies model =
    model.melody
        |> Array.map Array.toList
        |> Array.toList
        |> List.map chordToFrequencies


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCurrentStep step ->
            ( { model | activeChord = modBy 8 step }, Cmd.none )

        SetNote ( rowNumber, columnNumber, noteDefinition ) ->
            case Array.get columnNumber model.melody of
                Maybe.Just chord ->
                    let
                        updatedChord =
                            Array.set rowNumber noteDefinition chord
                    in
                    let
                        updatedMelody =
                            Array.set columnNumber updatedChord model.melody
                    in
                    let
                        updatedModel =
                            { model | melody = updatedMelody }
                    in
                    ( updatedModel, SoundEngineController.stepEngine (SoundEngineController.encode_melody (melodyFrequencies updatedModel)) )

                Maybe.Nothing ->
                    ( model, Cmd.none )

        UpdateOscillator NextWave ->
            let
                oscillator =
                    model.oscillator
            in
            ( { model | oscillator = { oscillator | wave = nextWave oscillator.wave } }, transmitWave (stringOfWave (nextWave oscillator.wave)) )

        UpdateOscillator PrevWave ->
            let
                oscillator =
                    model.oscillator
            in
            ( { model | oscillator = { oscillator | wave = prevWave oscillator.wave } }, transmitWave (stringOfWave (prevWave oscillator.wave)) )

        SetOctave octave ->
            ( { model | octave = octave }, transmitOctave octave )



-- PORTS


port transmitWave : String -> Cmd msg


port transmitOctave : Int -> Cmd msg
