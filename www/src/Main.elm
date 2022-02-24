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


type alias KickParams =
    { freq : Float
    , pitch : Float
    , wave : String
    , decay : Float
    , bite : Float
    }


type alias Model =
    { playing : Bool
    , value : String
    , kick : KickParams
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { playing = False
      , value = ""
      , kick =
            { freq = 40
            , pitch = 10
            , wave = "sin"
            , decay = 0.1
            , bite = 0.5
            }
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
    | Bite String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayKick ->
            ( model
            , playKick { freq = 40, pitch = 10, wave = "sin", decay = 0.1, bite = 0.5 }
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

        Bite value ->
            let
                floatValue =
                    parseString value

                kick =
                    model.kick
            in
            case floatValue of
                Just float ->
                    ( { model | value = value, kick = { kick | bite = float } }, updateKick { kick | bite = float } )

                Nothing ->
                    ( { model | value = value }, Cmd.none )


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

        -- , input [ A.type_ "range", A.min "30", A.max "80", A.step "0.1", onInput Slide ] []
        , text model.value
        , controls
        ]


controls : Html Msg
controls =
    div
        [ A.style "display" "flex"
        , A.style "flex-direction" "column"
        ]
        [ input [ A.type_ "range", A.min "30", A.max "80", A.step "0.1", onInput Freq ] []
        , input [ A.type_ "range", A.min "1", A.max "90", A.step "0.1", onInput Pitch ] []
        , input [ A.type_ "range", A.min "0.01", A.max "0.3", A.step "0.001", onInput Decay  ] []
        , input [ A.type_ "range", A.min "0", A.max "11", A.step "0.01", onInput Bite  ] []
        ]


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


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none


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
