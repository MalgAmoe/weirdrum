port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)


port playKick : KickParams -> Cmd msg


port playSequence : () -> Cmd msg


port stopSequence : () -> Cmd msg


port updateKick : KickParams -> Cmd msg


port receiveStepNumber : (Int -> msg) -> Sub msg


type alias KickParams =
    { freq : Float
    , pitch : Float
    , wave : String
    , decay : Float
    , attack : Float
    , volume : Float
    }


type alias Model =
    { playing : Bool
    , value : String
    , kick : KickParams
    , stepNumber : Int
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
            , volume = 0.1
            }
      , stepNumber = 0
      }
    , Cmd.none
    )


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


view : Model -> Html Msg
view model =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "background" "#e8e7e2"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid grey"
            , A.style "margin-right" "10px"
            ]
    in
    div
        [ A.style "width" "100%"
        , A.style "height" "100%"
        , A.style "font-family" "Helvetica, sans-serif"
        , A.style "line-height" "3.5em"
        , A.style "width" "50vw"
        , A.style "min-width" "350px"
        , A.style "margin" "auto"
        ]
        [ button
            (onClick PlayKick :: buttonStyles)
            [ text "KICKKKKK" ]
        , playingButton model.playing
        , text model.value
        , kickControls model.kick
        , text (String.fromInt model.stepNumber)
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
        , sliderWithValue "decay" kickParams.decay "0.01" "0.3" "0.001" Decay
        , sliderWithValue "attack" kickParams.attack "0" "2" "0.001" Attack
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
        , input [ A.style "width" "100%", A.type_ "range", A.min min, A.max max, A.step step, A.value (String.fromFloat value), onInput msg ] []
        ]


waveButton : Bool -> Html Msg
waveButton isSine =
    let
        buttonStyles =
            [ A.style "padding" "4px 12px"
            , A.style "background" "#e8e7e2"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid grey"
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
            , A.style "background" "#e8e7e2"
            , A.style "border-radius" "28px"
            , A.style "font-size" "0.8em"
            , A.style "border" "2px solid grey"
            , A.style "margin-right" "10px"
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



-- subscriptions : a -> Sub msg


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
