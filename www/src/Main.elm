port module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes as A exposing (disabled)
import Html.Events exposing (onBlur, onClick, onInput)
import Parser exposing (..)


port playKick : KickParams -> Cmd msg


port playSequence : () -> Cmd msg


port stopSequence : () -> Cmd msg


port updateKick : KickParams -> Cmd msg


port updateSequence : List KickParamsOut -> Cmd msg


port updateSequencerLength : Int -> Cmd msg


port updateOffset : Float -> Cmd msg

port updateTempo : Float -> Cmd msg


port receiveStepNumber : (Int -> msg) -> Sub msg


type Step
    = Trigger
    | LockTrigger KickParams
    | EmptyStep


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


type alias Model =
    { playing : Bool
    , kick : KickParams
    , kickEdit : Maybe KickParams
    , stepNumber : Int
    , steps : List Step
    , editing : Bool
    , editingStep : Maybe Int
    , sequencerLength : Int
    , offset : Int
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
      , stepNumber = 0
      , steps = emptySequencer
      , editing = False
      , editingStep = Nothing
      , sequencerLength = 16
      , offset = 0
      , tempo = "90"
      }
    , Cmd.none
    )


emptySequencer : List Step
emptySequencer =
    List.map (\_ -> EmptyStep) (List.range 0 15)


transformStep : KickParams -> Step -> KickParamsOut
transformStep kickParams step =
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

        LockTrigger params ->
            { freq = params.freq
            , pitch = params.pitch
            , wave = params.wave
            , decay = params.decay
            , punch = params.punch
            , volume = params.volume
            , step_type = "lock_trigger"
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


compileSteps : List Step -> KickParams -> KickParams -> Int -> ( List Step, List KickParamsOut )
compileSteps steps kick kickEdit stepNumber =
    let
        stepArray =
            Array.fromList steps

        newStep =
            LockTrigger kickEdit

        newSteps =
            Array.toList <| Array.set stepNumber newStep stepArray
    in
    ( newSteps, List.map (\a -> transformStep kick a) newSteps )


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


type Msg
    = PlayKick
    | PlaySequence
    | StopSequence
    | StepNumber Int
    | Steps Int
    | Move StepMove
    | ToggleEdit
    | UpdateParams KickParamsStrings
    | UpdateSequencerLength String
    | UpdateOffset Int
    | UpdateTempo String
    | FixTempo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayKick ->
            ( model
            , playKick { freq = 40, pitch = 10, wave = "sine", decay = 0.1, punch = 0.5, volume = 1 }
            )

        PlaySequence ->
            ( { model | playing = True }
            , playSequence ()
            )

        StopSequence ->
            ( { model | playing = False }
            , stopSequence ()
            )

        UpdateParams params ->
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
                    case model.editingStep of
                        Just stepNumber ->
                            let
                                ( steps, compiledSteps ) =
                                    compileSteps model.steps model.kick newKick stepNumber
                            in
                            ( { model | kickEdit = Just newKick, steps = steps }, updateSequence compiledSteps )

                        Nothing ->
                            ( { model | kickEdit = Just newKick }, Cmd.none )

                Nothing ->
                    ( { model | kick = newKick }, updateKick newKick )

        StepNumber step ->
            ( { model | stepNumber = step }, Cmd.none )

        Steps value ->
            let
                stepArray =
                    Array.fromList model.steps

                step =
                    Array.get value stepArray

                newStep =
                    case step of
                        Just el ->
                            case el of
                                EmptyStep ->
                                    case model.kickEdit of
                                        Just kickEdit ->
                                            LockTrigger kickEdit

                                        Nothing ->
                                            Trigger

                                _ ->
                                    EmptyStep

                        _ ->
                            EmptyStep

                newSteps =
                    Array.toList <| Array.set value newStep stepArray

                editingStep =
                    if model.editing then
                        Just value

                    else
                        Nothing
            in
            ( { model | steps = newSteps, editingStep = editingStep }, updateSequence (List.map (\a -> transformStep model.kick a) newSteps) )

        Move value ->
            let
                stepsArray =
                    Array.fromList model.steps

                start =
                    Array.toList <| Array.slice 0 model.sequencerLength stepsArray

                end =
                    Array.toList <| Array.slice model.sequencerLength 16 stepsArray

                newSteps =
                    (case value of
                        Left ->
                            rotateSteps start

                        Right ->
                            List.reverse <| rotateSteps (List.reverse start)
                    )
                        ++ end
            in
            ( { model | steps = newSteps, editingStep = Nothing }, updateSequence (List.map (\a -> transformStep model.kick a) newSteps) )

        ToggleEdit ->
            let
                editing =
                    not model.editing

                kickEdit =
                    if editing then
                        Just model.kick

                    else
                        Nothing
            in
            ( { model | editing = editing, kickEdit = kickEdit, editingStep = Nothing }, Cmd.none )

        UpdateSequencerLength lengthStr ->
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
            in
            ( { model | sequencerLength = length }, updateSequencerLength length )

        UpdateOffset value ->
            let
                offset =
                    clipValues
                        (model.offset + value)
                        -5
                        5

                offsetFloat =
                    toFloat offset * 0.01
            in
            ( { model | offset = offset }, updateOffset offsetFloat )

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


view : Model -> Html Msg
view model =
    let
        controls =
            case model.kickEdit of
                Just kickEdit ->
                    kickEdit

                Nothing ->
                    model.kick
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
        [ playingButton model.playing
        , input
            [ A.style "color" "yellow"
            , A.style "background-color" "black"
            , A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            , A.style "width" "30px"
            , A.style "margin-left" "5px"
            , A.style "text-align" "center"
            , A.value model.tempo
            , onInput UpdateTempo
            , onBlur (FixTempo model.tempo)
            ]
            []
        , kickControls controls
        , sequencerControls
            [ moveStepsButtons
            , editStepButton model.editing
            , input
                [ A.style "width" "150px"
                , A.style "background-color" "purple"
                , disabled model.playing
                , A.style "opacity"
                    (if model.playing then
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
                , A.value (String.fromInt model.sequencerLength)
                , onInput UpdateSequencerLength
                ]
                []
            , div
                [ A.style "display" "flex"
                , A.style "justify-content" "flex-start"
                , A.style "align-items" "flex-end"
                , A.style "margin-bottom" "2px"
                ]
                [ text (String.fromInt model.sequencerLength)
                ]
            ]
        , sequencerSteps model.steps model.stepNumber model.editingStep model.sequencerLength
        , offsetButtons model.offset
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
        [ waveButton (kickParams.wave == "sine") (\a -> UpdateParams { s | wave = a })
        , sliderWithValue "freq" kickParams.freq "30" "90" "0.1" (\a -> UpdateParams { s | freq = a })
        , sliderWithValue "pitch" kickParams.pitch "0" "30" "0.01" (\a -> UpdateParams { s | pitch = a })
        , sliderWithValue "punch" kickParams.punch "0" "2" "0.001" (\a -> UpdateParams { s | punch = a })
        , sliderWithValue "decay" kickParams.decay "0.01" "0.3" "0.001" (\a -> UpdateParams { s | decay = a })
        , sliderWithValue "volume" kickParams.volume "0.01" "1" "0.001" (\a -> UpdateParams { s | volume = a })
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
        , A.style "justify-content" "flex-start"
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
            , A.style "margin-bottom" "5px"
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


moveStepsButtons : Html Msg
moveStepsButtons =
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
        [ button (onClick (Move Left) :: buttonStyles) [ text "<" ]
        , button (onClick (Move Right) :: buttonStyles) [ text ">" ]
        ]


offsetButtons : Int -> Html Msg
offsetButtons offset =
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
        [ A.style "margin-top" "10px"
        ]
        [ button (onClick (UpdateOffset -1) :: buttonStyles) [ text "<" ]
        , span
            [ A.style "margin-left" "5px"
            , A.style "margin-right" "5px"
            ]
            [ text (String.fromInt offset ++ " ms") ]
        , button (onClick (UpdateOffset 1) :: buttonStyles) [ text ">" ]
        ]


editStepButton : Bool -> Html Msg
editStepButton editing =
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
    button (onClick ToggleEdit :: styleUsed) [ text "edit steps" ]


triggerStep : Array.Array Step -> Int -> Int -> Maybe Int -> Html Msg
triggerStep steps n stepPlaying editingStep =
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
    div (onClick (Steps n) :: basicStyle ++ style ++ triggerStyle) []


sequencerSteps : List Step -> Int -> Maybe Int -> Int -> Html Msg
sequencerSteps steps stepNumber editingStep sequencerLength =
    let
        list =
            List.range 0 (-1 + sequencerLength)

        stepsArray =
            Array.fromList steps

        elements =
            List.map (\n -> triggerStep stepsArray n stepNumber editingStep) list

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
    receiveStepNumber StepNumber


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- parser


parseString : String -> Maybe Float
parseString string =
    string
        |> Parser.run Parser.float
        |> Result.toMaybe
