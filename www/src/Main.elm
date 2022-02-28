port module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)


port playKick : KickParams -> Cmd msg


port playSequence : () -> Cmd msg


port stopSequence : () -> Cmd msg


port updateKick : KickParams -> Cmd msg


port updateSequence : List KickParamsOut -> Cmd msg


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
    , attack : Float
    , volume : Float
    }


type alias KickParamsOut =
    { freq : Float
    , pitch : Float
    , wave : String
    , decay : Float
    , attack : Float
    , volume : Float
    , step_type : String
    }


type alias Model =
    { playing : Bool
    , value : String
    , kick : KickParams
    , kickEdit : Maybe KickParams
    , stepNumber : Int
    , steps : List Step
    , editing : Bool
    , editingStep : Maybe Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { playing = False
      , value = ""
      , kick =
            { freq = 40
            , pitch = 10
            , wave = "sine"
            , decay = 0.1
            , attack = 0.5
            , volume = 0.5
            }
      , kickEdit = Nothing
      , stepNumber = 0
      , steps = emptySequencer
      , editing = False
      , editingStep = Nothing
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
            , attack = kickParams.attack
            , volume = kickParams.volume
            , step_type = "trigger"
            }

        LockTrigger params ->
            { freq = params.freq
            , pitch = params.pitch
            , wave = params.wave
            , decay = params.decay
            , attack = params.attack
            , volume = params.volume
            , step_type = "lock_trigger"
            }

        EmptyStep ->
            { freq = 0
            , pitch = 0
            , wave = ""
            , decay = 0
            , attack = 0
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


compileSteps steps kickEdit kick stepNumber =
    let
        stepArray =
            Array.fromList steps

        newStep =
            LockTrigger kickEdit

        newSteps =
            Array.toList <| Array.set stepNumber newStep stepArray
    in
    List.map (\a -> transformStep kick a) newSteps


type Msg
    = PlayKick
    | PlaySequence
    | StopSequence
    | Freq String
    | Pitch String
    | Decay String
    | Attack String
    | Volume String
    | Wave String
    | StepNumber Int
    | Steps Int
    | Move StepMove
    | ToggleEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayKick ->
            ( model
            , playKick { freq = 40, pitch = 10, wave = "sine", decay = 0.1, attack = 0.5, volume = 1 }
            )

        PlaySequence ->
            ( { model | playing = True }
            , playSequence ()
            )

        StopSequence ->
            ( { model | playing = False }
            , stopSequence ()
            )

        Freq value ->
            let
                floatValue =
                    parseString value

                kick =
                    model.kick
            in
            case model.kickEdit of
                Just kickEdit ->
                    case floatValue of
                        Just float ->
                            case model.editingStep of
                                Just stepNumber ->
                                    let
                                        steps =
                                            compileSteps model.steps kickEdit model.kick stepNumber
                                    in
                                    ( { model | value = value, kickEdit = Just { kickEdit | freq = float } }, updateSequence steps )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( { model | value = value }, Cmd.none )

                Nothing ->
                    case floatValue of
                        Just float ->
                            ( { model | value = value, kick = { kick | freq = float } }, updateKick { kick | freq = float } )

                        Nothing ->
                            ( { model | value = value }, Cmd.none )

        Pitch value ->
            let
                floatValue =
                    parseString value

                kick =
                    model.kick
            in
            case floatValue of
                Just float ->
                    ( { model | value = value, kick = { kick | pitch = float } }, updateKick { kick | pitch = float } )

                Nothing ->
                    ( { model | value = value }, Cmd.none )

        Decay value ->
            let
                floatValue =
                    parseString value

                kick =
                    model.kick
            in
            case floatValue of
                Just float ->
                    ( { model | value = value, kick = { kick | decay = float } }, updateKick { kick | decay = float } )

                Nothing ->
                    ( { model | value = value }, Cmd.none )

        Attack value ->
            let
                floatValue =
                    parseString value

                kick =
                    model.kick
            in
            case floatValue of
                Just float ->
                    ( { model | value = value, kick = { kick | attack = float } }, updateKick { kick | attack = float } )

                Nothing ->
                    ( { model | value = value }, Cmd.none )

        Volume value ->
            let
                floatValue =
                    parseString value

                kick =
                    model.kick
            in
            case floatValue of
                Just float ->
                    ( { model | value = value, kick = { kick | volume = float } }, updateKick { kick | volume = float } )

                Nothing ->
                    ( { model | value = value }, Cmd.none )

        Wave value ->
            let
                kick =
                    model.kick
            in
            ( { model | kick = { kick | wave = value } }, updateKick { kick | wave = value } )

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
                newSteps =
                    case value of
                        Left ->
                            rotateSteps model.steps

                        Right ->
                            List.reverse <| rotateSteps <| List.reverse model.steps
            in
            ( { model | steps = newSteps }, updateSequence (List.map (\a -> transformStep model.kick a) newSteps) )

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
        , A.style "min-width" "350px"
        , A.style "margin" "auto"
        , A.style "color" "yellow"
        , A.style "background-color" "black"
        ]
        [ playingButton model.playing
        , text model.value
        , kickControls controls
        , sequencerControls
            [ moveStepsButtons
            , editStepButton model.editing
            ]
        , sequencerSteps model.steps model.stepNumber model.editingStep
        ]


kickControls : KickParams -> Html Msg
kickControls kickParams =
    div
        [ A.style "display" "flex"
        , A.style "flex-direction" "column"
        ]
        [ waveButton (kickParams.wave == "sine")
        , sliderWithValue "freq" kickParams.freq "30" "90" "0.1" Freq
        , sliderWithValue "pitch" kickParams.pitch "0" "30" "0.01" Pitch
        , sliderWithValue "attack" kickParams.attack "0" "2" "0.001" Attack
        , sliderWithValue "decay" kickParams.decay "0.01" "0.3" "0.001" Decay
        , sliderWithValue "volume" kickParams.volume "0" "1" "0.001" Volume
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
        , A.style "margin-bottom" "5px"
        ]
        child


waveButton : Bool -> Html Msg
waveButton isSine =
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
            (onClick (Wave "triangle") :: buttonStyles)
            [ text "sine" ]

    else
        button
            (onClick (Wave "sine") :: buttonStyles)
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


sequencerSteps : List Step -> Int -> Maybe Int -> Html Msg
sequencerSteps steps stepNumber editingStep =
    let
        list =
            List.range 0 15

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
