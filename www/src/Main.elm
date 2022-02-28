port module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes as A
import Html.Events exposing (on, onClick, onInput)
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
    , stepNumber : Int
    , steps : List Step
    , editing : Bool
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
      , stepNumber = 0
      , steps = emptySequencer
      , editing = False
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
                                    Trigger

                                _ ->
                                    EmptyStep

                        _ ->
                            EmptyStep

                newSteps =
                    Array.toList <| Array.set value newStep stepArray
            in
            ( { model | steps = newSteps }, updateSequence (List.map (\a -> transformStep model.kick a) newSteps) )

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
            ( { model | editing = not model.editing }, Cmd.none )


view : Model -> Html Msg
view model =
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
        , kickControls model.kick
        , sequencerControls
            [ moveStepsButtons
            , editStepButton model.editing
            ]
        , sequencerSteps model.steps model.stepNumber
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
            [ A.style "background" "purple"
            , A.style "border" "2px solid yellow"
            ]

        unactive =
            [ A.style "background" "black"
            , A.style "border" "2px solid purple"
            ]

        basic =
            [ A.style "padding" "4px 12px"
            , A.style "color" "yellow"
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


triggerStep : Array.Array Step -> Int -> Int -> Html Msg
triggerStep steps n stepPlaying =
    let
        isPlaying =
            n == stepPlaying

        hasTrigger =
            case Array.get n steps of
                Just el ->
                    case el of
                        EmptyStep ->
                            False

                        _ ->
                            True

                _ ->
                    False

        normalStyles =
            [ A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid purple"
            , A.style "margin-right" "10px"
            , A.style "width" "30px"
            , A.style "height" "30px"
            ]

        playingStyles =
            [ A.style "padding" "4px 12px"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid yellow"
            , A.style "margin-right" "10px"
            , A.style "width" "30px"
            , A.style "height" "30px"
            ]

        hasTriggerStyle =
            if hasTrigger then
                [ A.style "background" "purple" ]

            else
                [ A.style "background" "black" ]

        style =
            if isPlaying then
                playingStyles

            else
                normalStyles
    in
    div (onClick (Steps n) :: style ++ hasTriggerStyle) []


sequencerSteps : List Step -> Int -> Html Msg
sequencerSteps steps stepNumber =
    let
        list =
            List.range 0 15

        stepsArray =
            Array.fromList steps

        elements =
            List.map (\n -> triggerStep stepsArray n stepNumber) list

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
