port module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as A exposing (disabled)
import Html.Events exposing (onBlur, onClick, onInput)
import Parser exposing (..)



-- PORTS


port playSequence : () -> Cmd msg


port stopSequence : () -> Cmd msg


port updateKick : KickParams -> Cmd msg


port updateSnare : SnareParams -> Cmd msg


port updateKickSequence : List KickParamsOut -> Cmd msg


port updateSnareSequence : List SnareParamsOut -> Cmd msg


port updateKickSequencerLength : Int -> Cmd msg


port updateSnareSequencerLength : Int -> Cmd msg


port updateKickOffset : Float -> Cmd msg


port updateSnareOffset : Float -> Cmd msg


port updateTempo : Float -> Cmd msg


port receiveKickStepNumber : (Int -> msg) -> Sub msg


port receiveSnareStepNumber : (Int -> msg) -> Sub msg



-- TYPES


type Step
    = Trigger
    | LockTrigger Sound
    | EmptyStep


type Sound
    = KickSound KickParams
    | SnareSound SnareParams


type StepMove
    = Left
    | Right


type alias KickParams =
    { freq : Float
    , pitch : Float
    , wave : String
    , decay : Float
    , punch : Float
    , volume : Float
    }


type alias KickParamsOut =
    { freq : Float
    , pitch : Float
    , wave : String
    , decay : Float
    , punch : Float
    , volume : Float
    , step_type : String
    }


type alias KickParamsStrings =
    { freq : String
    , pitch : String
    , wave : String
    , decay : String
    , punch : String
    , volume : String
    }


type alias SnareParams =
    { freq : Float
    , blend : Float
    , decay : Float
    , punch : Float
    , volume : Float
    }


type alias SnareParamsOut =
    { freq : Float
    , blend : Float
    , decay : Float
    , punch : Float
    , volume : Float
    , step_type : String
    }


type alias SnareParamsStrings =
    { freq : String
    , blend : String
    , decay : String
    , punch : String
    , volume : String
    }


type alias Sequencer =
    { stepNumber : Int
    , steps : List Step
    , editing : Bool
    , editingStep : Maybe Int
    , sequencerLength : Int
    , offset : Int
    }



-- MODEL


type alias Model =
    { playing : Bool
    , kick : KickParams
    , kickEdit : Maybe KickParams
    , kickSequencer : Sequencer
    , snare : SnareParams
    , snareEdit : Maybe SnareParams
    , snareSequencer : Sequencer
    , tempo : String
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { playing = False
      , kick =
            { freq = 40
            , pitch = 10
            , wave = "sine"
            , decay = 0.1
            , punch = 0.5
            , volume = 0.5
            }
      , kickEdit = Nothing
      , kickSequencer =
            { stepNumber = 0
            , steps = emptySequencer
            , editing = False
            , editingStep = Nothing
            , sequencerLength = 16
            , offset = 0
            }
      , snare =
            { freq = 120
            , blend = 0.5
            , decay = 0.1
            , punch = 1.0
            , volume = 0.5
            }
      , snareEdit = Nothing
      , snareSequencer =
            { stepNumber = 0
            , steps = emptySequencer
            , editing = False
            , editingStep = Nothing
            , sequencerLength = 16
            , offset = 0
            }
      , tempo = "90"
      }
    , Cmd.none
    )



-- HELPERS


emptySequencer : List Step
emptySequencer =
    List.map (\_ -> EmptyStep) (List.range 0 15)


transformKickStep : KickParams -> Step -> KickParamsOut
transformKickStep kickParams step =
    case step of
        Trigger ->
            { freq = kickParams.freq
            , pitch = kickParams.pitch
            , wave = kickParams.wave
            , decay = kickParams.decay
            , punch = kickParams.punch
            , volume = kickParams.volume
            , step_type = "trigger"
            }

        LockTrigger sound ->
            case sound of
                KickSound kick ->
                    { freq = kick.freq
                    , pitch = kick.pitch
                    , wave = kick.wave
                    , decay = kick.decay
                    , punch = kick.punch
                    , volume = kick.volume
                    , step_type = "lock_trigger"
                    }

                _ ->
                    { freq = 0
                    , pitch = 0
                    , wave = ""
                    , decay = 0
                    , punch = 0
                    , volume = 0
                    , step_type = "empty"
                    }

        EmptyStep ->
            { freq = 0
            , pitch = 0
            , wave = ""
            , decay = 0
            , punch = 0
            , volume = 0
            , step_type = "empty"
            }


transformSnareStep : SnareParams -> Step -> SnareParamsOut
transformSnareStep snareParams step =
    case step of
        Trigger ->
            { freq = snareParams.freq
            , blend = snareParams.blend
            , decay = snareParams.decay
            , punch = snareParams.punch
            , volume = snareParams.volume
            , step_type = "trigger"
            }

        LockTrigger sound ->
            case sound of
                SnareSound snare ->
                    { freq = snare.freq
                    , blend = snare.blend
                    , decay = snare.decay
                    , punch = snare.punch
                    , volume = snare.volume
                    , step_type = "lock_trigger"
                    }

                _ ->
                    { freq = 0
                    , blend = 0
                    , decay = 0
                    , punch = 0
                    , volume = 0
                    , step_type = "empty"
                    }

        EmptyStep ->
            { freq = 0
            , blend = 0
            , decay = 0
            , punch = 0
            , volume = 0
            , step_type = "empty"
            }


rotateSteps : List Step -> List Step
rotateSteps steps =
    let
        head =
            List.head steps

        tail =
            List.tail steps

        newSteps =
            case head of
                Just step ->
                    case tail of
                        Just list ->
                            List.append list [ step ]

                        _ ->
                            emptySequencer

                _ ->
                    emptySequencer
    in
    newSteps


compileKickSteps : List Step -> KickParams -> KickParams -> Int -> ( List Step, List KickParamsOut )
compileKickSteps steps kick kickEdit stepNumber =
    let
        stepArray =
            Array.fromList steps

        newStep =
            LockTrigger (KickSound kickEdit)

        newSteps =
            Array.toList <| Array.set stepNumber newStep stepArray
    in
    ( newSteps, List.map (\a -> transformKickStep kick a) newSteps )


compileSnareSteps : List Step -> SnareParams -> SnareParams -> Int -> ( List Step, List SnareParamsOut )
compileSnareSteps steps snare snareEdit stepNumber =
    let
        stepArray =
            Array.fromList steps

        newStep =
            LockTrigger (SnareSound snareEdit)

        newSteps =
            Array.toList <| Array.set stepNumber newStep stepArray
    in
    ( newSteps, List.map (\a -> transformSnareStep snare a) newSteps )


clipValues : comparable -> comparable -> comparable -> comparable
clipValues value min max =
    value
        |> (\a ->
                if a > max then
                    max

                else if a < min then
                    min

                else
                    a
           )


addValueToString : String -> Int -> String
addValueToString string value =
    case String.toInt string of
        Just stringInt ->
            String.fromInt (stringInt + value)

        Nothing ->
            string



-- MSGS


type Msg
    = PlaySequence
    | StopSequence
    | KickStepNumber Int
    | SnareStepNumber Int
    | KickSteps Int
    | SnareSteps Int
    | MoveKick StepMove
    | MoveSnare StepMove
    | ToggleKickEdit
    | ToggleSnareEdit
    | UpdateSnareParams SnareParamsStrings
    | UpdateKickParams KickParamsStrings
    | UpdateKickSequencerLength String
    | UpdateSnareSequencerLength String
    | UpdateKickOffset Int
    | UpdateSnareOffset Int
    | UpdateTempo String
    | FixTempo String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaySequence ->
            ( { model | playing = True }
            , playSequence ()
            )

        StopSequence ->
            ( { model | playing = False }
            , stopSequence ()
            )

        UpdateKickParams params ->
            let
                freq =
                    case parseString params.freq of
                        Just value ->
                            clipValues value 30 90

                        Nothing ->
                            model.kick.freq

                pitch =
                    case parseString params.pitch of
                        Just value ->
                            clipValues value 0 30

                        Nothing ->
                            model.kick.pitch

                punch =
                    case parseString params.punch of
                        Just value ->
                            clipValues value 0 2

                        Nothing ->
                            model.kick.punch

                decay =
                    case parseString params.decay of
                        Just value ->
                            clipValues value 0.01 0.3

                        Nothing ->
                            model.kick.decay

                volume =
                    case parseString params.volume of
                        Just value ->
                            clipValues value 0.01 1

                        Nothing ->
                            model.kick.volume

                newKick =
                    { wave = params.wave, freq = freq, pitch = pitch, punch = punch, decay = decay, volume = volume }
            in
            case model.kickEdit of
                Just _ ->
                    case model.kickSequencer.editingStep of
                        Just stepNumber ->
                            let
                                kickSequencer =
                                    model.kickSequencer

                                ( steps, compiledSteps ) =
                                    compileKickSteps kickSequencer.steps model.kick newKick stepNumber
                            in
                            ( { model | kickEdit = Just newKick, kickSequencer = { kickSequencer | steps = steps } }, updateKickSequence compiledSteps )

                        Nothing ->
                            ( { model | kickEdit = Just newKick }, Cmd.none )

                Nothing ->
                    ( { model | kick = newKick }, updateKick newKick )

        UpdateSnareParams params ->
            let
                freq =
                    case parseString params.freq of
                        Just value ->
                            clipValues value 100 300

                        Nothing ->
                            model.snare.freq

                blend =
                    case parseString params.blend of
                        Just value ->
                            clipValues value 0 1

                        Nothing ->
                            model.snare.blend

                punch =
                    case parseString params.punch of
                        Just value ->
                            clipValues value 0 2

                        Nothing ->
                            model.snare.punch

                decay =
                    case parseString params.decay of
                        Just value ->
                            clipValues value 0.01 0.3

                        Nothing ->
                            model.snare.decay

                volume =
                    case parseString params.volume of
                        Just value ->
                            clipValues value 0.01 1

                        Nothing ->
                            model.snare.volume

                newSnare =
                    { blend = blend, freq = freq, punch = punch, decay = decay, volume = volume }
            in
            case model.snareEdit of
                Just _ ->
                    case model.snareSequencer.editingStep of
                        Just stepNumber ->
                            let
                                snareSequencer =
                                    model.snareSequencer

                                ( steps, compiledSteps ) =
                                    compileSnareSteps snareSequencer.steps model.snare newSnare stepNumber
                            in
                            ( { model | snareEdit = Just newSnare, snareSequencer = { snareSequencer | steps = steps } }, updateSnareSequence compiledSteps )

                        Nothing ->
                            ( { model | snareEdit = Just newSnare }, Cmd.none )

                Nothing ->
                    ( { model | snare = newSnare }, updateSnare newSnare )

        KickStepNumber step ->
            let
                kickSequencer =
                    model.kickSequencer
            in
            ( { model | kickSequencer = { kickSequencer | stepNumber = step } }, Cmd.none )

        SnareStepNumber step ->
            let
                snareSequencer =
                    model.snareSequencer
            in
            ( { model | snareSequencer = { snareSequencer | stepNumber = step } }, Cmd.none )

        KickSteps value ->
            let
                stepArray =
                    Array.fromList model.kickSequencer.steps

                step =
                    Array.get value stepArray

                ( newStep, kickEdit, editingStep ) =
                    case step of
                        Just el ->
                            if model.kickSequencer.editing then
                                case el of
                                    EmptyStep ->
                                        case model.kickEdit of
                                            Just kickEditValue ->
                                                ( LockTrigger (KickSound kickEditValue), model.kickEdit, Just value )

                                            Nothing ->
                                                ( Trigger, Nothing, Nothing )

                                    Trigger ->
                                        ( LockTrigger (KickSound model.kick), Just model.kick, Just value )

                                    LockTrigger sound ->
                                        case sound of
                                            KickSound kickEditValue ->
                                                case model.kickSequencer.editingStep of
                                                    Just stepNumber ->
                                                        if stepNumber == value then
                                                            ( EmptyStep, Just kickEditValue, Nothing )

                                                        else
                                                            ( LockTrigger sound, Just kickEditValue, Just value )

                                                    Nothing ->
                                                        ( LockTrigger sound, Just kickEditValue, Just value )

                                            _ ->
                                                ( Trigger, Nothing, Nothing )

                            else
                                case el of
                                    EmptyStep ->
                                        ( Trigger, Nothing, Nothing )

                                    Trigger ->
                                        ( EmptyStep, Nothing, Nothing )

                                    LockTrigger _ ->
                                        ( EmptyStep, Nothing, Nothing )

                        _ ->
                            ( EmptyStep, Nothing, Nothing )

                newSteps =
                    Array.toList <| Array.set value newStep stepArray

                kickSequencer =
                    model.kickSequencer
            in
            ( { model | kickSequencer = { kickSequencer | steps = newSteps, editingStep = editingStep }, kickEdit = kickEdit }, updateKickSequence (List.map (\a -> transformKickStep model.kick a) newSteps) )

        SnareSteps value ->
            let
                stepArray =
                    Array.fromList model.snareSequencer.steps

                step =
                    Array.get value stepArray

                ( newStep, snareEdit, editingStep ) =
                    case step of
                        Just el ->
                            if model.snareSequencer.editing then
                                case el of
                                    EmptyStep ->
                                        case model.snareEdit of
                                            Just snareEditValue ->
                                                ( LockTrigger (SnareSound snareEditValue), model.snareEdit, Just value )

                                            Nothing ->
                                                ( Trigger, Nothing, Nothing )

                                    Trigger ->
                                        ( LockTrigger (SnareSound model.snare), Just model.snare, Just value )

                                    LockTrigger sound ->
                                        case sound of
                                            SnareSound snareEditValue ->
                                                case model.snareSequencer.editingStep of
                                                    Just stepNumber ->
                                                        if stepNumber == value then
                                                            ( EmptyStep, Just snareEditValue, Nothing )

                                                        else
                                                            ( LockTrigger sound, Just snareEditValue, Just value )

                                                    Nothing ->
                                                        ( LockTrigger sound, Just snareEditValue, Just value )

                                            _ ->
                                                ( Trigger, Nothing, Nothing )

                            else
                                case el of
                                    EmptyStep ->
                                        ( Trigger, Nothing, Nothing )

                                    Trigger ->
                                        ( EmptyStep, Nothing, Nothing )

                                    LockTrigger _ ->
                                        ( EmptyStep, Nothing, Nothing )

                        _ ->
                            ( EmptyStep, Nothing, Nothing )

                newSteps =
                    Array.toList <| Array.set value newStep stepArray

                snareSequencer =
                    model.snareSequencer
            in
            ( { model | snareSequencer = { snareSequencer | steps = newSteps, editingStep = editingStep }, snareEdit = snareEdit }, updateSnareSequence (List.map (\a -> transformSnareStep model.snare a) newSteps) )

        MoveKick value ->
            let
                stepsArray =
                    Array.fromList model.kickSequencer.steps

                start =
                    Array.toList <| Array.slice 0 model.kickSequencer.sequencerLength stepsArray

                end =
                    Array.toList <| Array.slice model.kickSequencer.sequencerLength 16 stepsArray

                newSteps =
                    (case value of
                        Left ->
                            rotateSteps start

                        Right ->
                            List.reverse <| rotateSteps (List.reverse start)
                    )
                        ++ end

                kickSequencer =
                    model.kickSequencer
            in
            ( { model | kickSequencer = { kickSequencer | steps = newSteps, editingStep = Nothing } }, updateKickSequence (List.map (\a -> transformKickStep model.kick a) newSteps) )

        MoveSnare value ->
            let
                stepsArray =
                    Array.fromList model.snareSequencer.steps

                start =
                    Array.toList <| Array.slice 0 model.snareSequencer.sequencerLength stepsArray

                end =
                    Array.toList <| Array.slice model.snareSequencer.sequencerLength 16 stepsArray

                newSteps =
                    (case value of
                        Left ->
                            rotateSteps start

                        Right ->
                            List.reverse <| rotateSteps (List.reverse start)
                    )
                        ++ end

                snareSequencer =
                    model.snareSequencer
            in
            ( { model | snareSequencer = { snareSequencer | steps = newSteps, editingStep = Nothing } }, updateSnareSequence (List.map (\a -> transformSnareStep model.snare a) newSteps) )

        ToggleKickEdit ->
            let
                editing =
                    not model.kickSequencer.editing

                kickEdit =
                    if editing then
                        Just model.kick

                    else
                        Nothing

                kickSequencer =
                    model.kickSequencer
            in
            ( { model | kickSequencer = { kickSequencer | editing = editing, editingStep = Nothing }, kickEdit = kickEdit }, Cmd.none )

        ToggleSnareEdit ->
            let
                editing =
                    not model.snareSequencer.editing

                snareEdit =
                    if editing then
                        Just model.snare

                    else
                        Nothing

                snareSequencer =
                    model.snareSequencer
            in
            ( { model | snareSequencer = { snareSequencer | editing = editing, editingStep = Nothing }, snareEdit = snareEdit }, Cmd.none )

        UpdateKickSequencerLength lengthStr ->
            let
                length =
                    case
                        lengthStr
                            |> Parser.run Parser.int
                            |> Result.toMaybe
                    of
                        Just value ->
                            clipValues value 2 16

                        Nothing ->
                            16

                kickSequencer =
                    model.kickSequencer
            in
            ( { model | kickSequencer = { kickSequencer | sequencerLength = length } }, updateKickSequencerLength length )

        UpdateSnareSequencerLength lengthStr ->
            let
                length =
                    case
                        lengthStr
                            |> Parser.run Parser.int
                            |> Result.toMaybe
                    of
                        Just value ->
                            clipValues value 2 16

                        Nothing ->
                            16

                snareSequencer =
                    model.snareSequencer
            in
            ( { model | snareSequencer = { snareSequencer | sequencerLength = length } }, updateSnareSequencerLength length )

        UpdateKickOffset value ->
            let
                offset =
                    clipValues
                        (model.kickSequencer.offset + value)
                        0
                        10

                offsetFloat =
                    toFloat offset * 0.01

                kickSequencer =
                    model.kickSequencer
            in
            ( { model | kickSequencer = { kickSequencer | offset = offset } }, updateKickOffset offsetFloat )

        UpdateSnareOffset value ->
            let
                offset =
                    clipValues
                        (model.snareSequencer.offset + value)
                        0
                        10

                offsetFloat =
                    toFloat offset * 0.01

                snareSequencer =
                    model.snareSequencer
            in
            ( { model | snareSequencer = { snareSequencer | offset = offset } }, updateSnareOffset offsetFloat )

        UpdateTempo tempoStr ->
            let
                isInt =
                    case String.toInt tempoStr of
                        Just _ ->
                            True

                        Nothing ->
                            False

                isFloat =
                    case String.toFloat tempoStr of
                        Just _ ->
                            True

                        Nothing ->
                            False

                isEmpty =
                    String.isEmpty tempoStr
            in
            if isInt || isFloat || isEmpty then
                ( { model | tempo = tempoStr }, Cmd.none )

            else
                ( model, Cmd.none )

        FixTempo tempoStr ->
            let
                ( isInt, tempoInt ) =
                    case String.toInt tempoStr of
                        Just value ->
                            ( True, clipValues value 30 270 )

                        Nothing ->
                            ( False, 90 )

                ( isFloat, tempoFloat ) =
                    case String.toFloat tempoStr of
                        Just value ->
                            ( True, clipValues value 30 270 )

                        Nothing ->
                            ( False, 90 )
            in
            if isInt then
                ( { model | tempo = String.fromInt tempoInt }, updateTempo <| toFloat tempoInt )

            else if isFloat then
                ( { model | tempo = String.fromInt <| floor tempoFloat }, updateTempo tempoFloat )

            else
                ( model, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    let
        controlsKick =
            case model.kickEdit of
                Just kickEdit ->
                    kickEdit

                Nothing ->
                    model.kick

        controlsSnare =
            case model.snareEdit of
                Just snareEdit ->
                    snareEdit

                Nothing ->
                    model.snare
    in
    div
        [ A.style "width" "100%"
        , A.style "height" "100%"
        , A.style "font-family" "Helvetica, sans-serif"
        , A.style "width" "50vw"
        , A.style "min-width" "700px"
        , A.style "margin" "auto"
        , A.style "color" "yellow"
        , A.style "background-color" "black"
        ]
        [ div
            [ A.style "display" "flex"
            , A.style "justify-content" "flex-start"
            , A.style "align-items" "center"
            , A.style "margin-bottom" "5px"
            ]
            [ playingButton model.playing
            , tempoControl model.tempo
            ]
        , soundWrapper
            [ div
                [ A.style "text-align" "center"
                , A.style "color" "purple"
                ]
                [ text "tac" ]
            , snareControls controlsSnare
            , lineSpace
            , sequencerControls
                [ moveStepsButtons MoveSnare
                , offsetButtons UpdateSnareOffset model.snareSequencer.offset
                , editStepButton model.snareSequencer.editing ToggleSnareEdit
                , sequencerLengthControl model.snareSequencer.sequencerLength model.playing UpdateSnareSequencerLength
                ]
            , sequencerSteps model.snareSequencer.steps model.snareSequencer.stepNumber model.snareSequencer.editingStep model.snareSequencer.sequencerLength SnareSteps
            ]
        , soundWrapper
            [ div
                [ A.style "text-align" "center"
                , A.style "color" "purple"
                ]
                [ text "tung" ]
            , kickControls controlsKick
            , lineSpace
            , sequencerControls
                [ moveStepsButtons MoveKick
                , offsetButtons UpdateKickOffset model.kickSequencer.offset
                , editStepButton model.kickSequencer.editing ToggleKickEdit
                , sequencerLengthControl model.kickSequencer.sequencerLength model.playing UpdateKickSequencerLength
                ]
            , sequencerSteps model.kickSequencer.steps model.kickSequencer.stepNumber model.kickSequencer.editingStep model.kickSequencer.sequencerLength KickSteps
            ]
        ]



-- SUB VIEWS


soundWrapper : List (Html msg) -> Html msg
soundWrapper =
    div
        [ A.style "border" "2px solid purple"
        , A.style "border-radius" "5px"
        , A.style "padding" "5px"
        , A.style "margin" "5px"
        ]


tempoControl : String -> Html Msg
tempoControl tempo =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "color" "yellow"
            , A.style "background" "black"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            ]
    in
    div
        [ A.style "margin-left" "5px"
        , A.style "display" "flex"
        , A.style "align-items" "center"
        ]
        [ button (onClick (FixTempo <| addValueToString tempo -1) :: buttonStyles) [ text "<" ]
        , input
            [ A.style "color" "yellow"
            , A.style "background-color" "black"
            , A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "border" "2px solid purple"
            , A.style "font-size" "0.8em"
            , A.style "width" "30px"
            , A.style "text-align" "center"
            , A.value tempo
            , onInput UpdateTempo
            , onBlur (FixTempo tempo)
            ]
            []
        , button (onClick (FixTempo <| addValueToString tempo 1) :: buttonStyles) [ text ">" ]
        ]


kickControls : KickParams -> Html Msg
kickControls kickParams =
    let
        s =
            { freq = String.fromFloat kickParams.freq
            , pitch = String.fromFloat kickParams.pitch
            , wave = kickParams.wave
            , decay = String.fromFloat kickParams.decay
            , punch = String.fromFloat kickParams.punch
            , volume = String.fromFloat kickParams.volume
            }
    in
    div
        [ A.style "display" "flex"
        , A.style "flex-direction" "column"
        ]
        [ waveButton (kickParams.wave == "sine") (\a -> UpdateKickParams { s | wave = a })
        , sliderWithValue "freq" kickParams.freq "30" "90" "0.1" (\a -> UpdateKickParams { s | freq = a })
        , sliderWithValue "pitch" kickParams.pitch "0" "30" "0.01" (\a -> UpdateKickParams { s | pitch = a })
        , sliderWithValue "punch" kickParams.punch "0" "2" "0.001" (\a -> UpdateKickParams { s | punch = a })
        , sliderWithValue "decay" kickParams.decay "0.01" "0.3" "0.001" (\a -> UpdateKickParams { s | decay = a })
        , sliderWithValue "volume" kickParams.volume "0.01" "1" "0.001" (\a -> UpdateKickParams { s | volume = a })
        ]


lineSpace : Html msg
lineSpace =
    div
        [ A.style "display" "flex"
        , A.style "align-items" "center"
        ]
        []


snareControls : SnareParams -> Html Msg
snareControls snareParams =
    let
        s =
            { freq = String.fromFloat snareParams.freq
            , blend = String.fromFloat snareParams.blend
            , decay = String.fromFloat snareParams.decay
            , punch = String.fromFloat snareParams.punch
            , volume = String.fromFloat snareParams.volume
            }
    in
    div
        [ A.style "display" "flex"
        , A.style "flex-direction" "column"
        ]
        [ sliderWithValue "freq" snareParams.freq "100" "300" "0.1" (\a -> UpdateSnareParams { s | freq = a })
        , sliderWithValue "blend" snareParams.blend "0" "1" "0.001" (\a -> UpdateSnareParams { s | blend = a })
        , sliderWithValue "punch" snareParams.punch "0" "2" "0.001" (\a -> UpdateSnareParams { s | punch = a })
        , sliderWithValue "decay" snareParams.decay "0.01" "0.3" "0.001" (\a -> UpdateSnareParams { s | decay = a })
        , sliderWithValue "volume" snareParams.volume "0.01" "1" "0.001" (\a -> UpdateSnareParams { s | volume = a })
        ]


sliderWithValue : String -> Float -> String -> String -> String -> (String -> msg) -> Html msg
sliderWithValue name value min max step msg =
    div
        [ A.style "display" "flex"
        , A.style "flex-direction" "row"
        , A.style "justify-content" "space-evenly"
        , A.style "height" "30px"
        ]
        [ div
            [ A.style "display" "flex"
            , A.style "flex-direction" "row"
            , A.style "justify-content" "space-between"
            , A.style "width" "120px"
            , A.style "line-height" "30px"
            ]
            [ span [] [ text name ]
            , span [ A.style "width" "50px", A.style "text-align" "center" ] [ text (String.fromFloat value) ]
            ]
        , input
            [ A.style "width" "100%"
            , A.style "background-color" "purple"
            , A.type_ "range"
            , A.style "margin-top" "9px"
            , A.min min
            , A.max max
            , A.step step
            , A.value (String.fromFloat value)
            , onInput msg
            ]
            []
        ]


sequencerControls : List (Html Msg) -> Html Msg
sequencerControls child =
    div
        [ A.style "display" "flex"
        , A.style "flex-direction" "row"
        , A.style "justify-content" "space-between"
        , A.style "align-items" "center"
        , A.style "margin-bottom" "10px"
        ]
        child


waveButton : Bool -> (String -> Msg) -> Html Msg
waveButton isSine msg =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "color" "yellow"
            , A.style "background" "black"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            , A.style "margin-bottom" "5px"
            ]
    in
    if isSine then
        button
            (onClick (msg "triangle") :: buttonStyles)
            [ text "sine" ]

    else
        button
            (onClick (msg "sine") :: buttonStyles)
            [ text "triangle" ]


playingButton : Bool -> Html Msg
playingButton isPlaying =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "color" "yellow"
            , A.style "background" "black"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            ]
    in
    if isPlaying then
        button
            (onClick StopSequence :: buttonStyles)
            [ text "Stop" ]

    else
        button
            (onClick PlaySequence :: buttonStyles)
            [ text "Play" ]


moveStepsButtons : (StepMove -> Msg) -> Html Msg
moveStepsButtons msg =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "color" "yellow"
            , A.style "background" "black"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            ]
    in
    div []
        [ button (onClick (msg Left) :: buttonStyles) [ text "<" ]
        , button (onClick (msg Right) :: buttonStyles) [ text ">" ]
        ]


offsetButtons : (Int -> Msg) -> Int -> Html Msg
offsetButtons msg offset =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "color" "yellow"
            , A.style "background" "black"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            ]
    in
    div
        []
        [ button (onClick (msg -1) :: buttonStyles) [ text "<" ]
        , span
            [ A.style "margin-left" "5px"
            , A.style "margin-right" "5px"
            ]
            [ text (String.fromInt offset ++ " ms") ]
        , button (onClick (msg 1) :: buttonStyles) [ text ">" ]
        ]


editStepButton : Bool -> Msg -> Html Msg
editStepButton editing msg =
    let
        active =
            [ A.style "background" "yellow"
            , A.style "color" "purple"
            , A.style "border" "2px solid purple"
            ]

        unactive =
            [ A.style "background" "black"
            , A.style "color" "yellow"
            , A.style "border" "2px solid purple"
            ]

        basic =
            [ A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "margin-left" "5px"
            ]

        styleUsed =
            if editing then
                basic ++ active

            else
                basic ++ unactive
    in
    button (onClick msg :: styleUsed) [ text "edit steps" ]


sequencerLengthControl : Int -> Bool -> (String -> Msg) -> Html Msg
sequencerLengthControl length playing msg =
    div
        [ A.style "display" "flex"
        , A.style "align-items" "center"
        ]
        [ input
            [ A.style "width" "150px"
            , A.style "background-color" "purple"
            , disabled playing
            , A.style "opacity"
                (if playing then
                    "0.5"

                 else
                    "1"
                )
            , A.style "margin" "none"
            , A.style "margin-left" "5px"
            , A.type_ "range"
            , A.min "2"
            , A.max "16"
            , A.step "1"
            , A.value (String.fromInt length)
            , onInput msg
            ]
            []
        , div
            [ A.style "text-align" "center"
            , A.style "margin-left" "5px"
            , A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "border" "2px solid purple"
            , A.style "font-size" "0.8em"
            , A.style "text-align" "center"
            , A.style "width" "30px"
            ]
            [ text (String.fromInt length)
            ]
        ]


triggerStep : Array.Array Step -> Int -> Int -> Maybe Int -> (Int -> Msg) -> Html Msg
triggerStep steps n stepPlaying editingStep msg =
    let
        isPlaying =
            n == stepPlaying

        isEditingStep =
            case editingStep of
                Just value ->
                    value == n

                Nothing ->
                    False

        basicStyle =
            [ A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "margin-right" "10px"
            , A.style "width" "30px"
            , A.style "height" "30px"
            ]

        normalStyles =
            [ A.style "border" "2px solid purple"
            ]

        playingStyles =
            [ A.style "border" "2px solid yellow"
            ]

        style =
            if isPlaying then
                playingStyles

            else
                normalStyles

        triggerStyle =
            case Array.get n steps of
                Just el ->
                    case el of
                        EmptyStep ->
                            [ A.style "background" "black" ]

                        Trigger ->
                            [ A.style "background" "purple" ]

                        LockTrigger _ ->
                            if isEditingStep then
                                [ A.style "background" "yellow" ]

                            else
                                [ A.style "background" "fuchsia" ]

                _ ->
                    [ A.style "background" "black" ]
    in
    div (onClick (msg n) :: basicStyle ++ style ++ triggerStyle) []


sequencerSteps : List Step -> Int -> Maybe Int -> Int -> (Int -> Msg) -> Html Msg
sequencerSteps steps stepNumber editingStep sequencerLength msg =
    let
        list =
            List.range 0 (-1 + sequencerLength)

        stepsArray =
            Array.fromList steps

        elements =
            List.map (\n -> triggerStep stepsArray n stepNumber editingStep msg) list

        style =
            [ A.style "display" "flex"
            , A.style "flex-direction" "row"
            , A.style "justify-content" "space-evenly"
            , A.style "width" "100%"
            ]
    in
    div style elements


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveKickStepNumber KickStepNumber
        , receiveSnareStepNumber SnareStepNumber
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PARSER


parseString : String -> Maybe Float
parseString string =
    string
        |> Parser.run Parser.float
        |> Result.toMaybe
