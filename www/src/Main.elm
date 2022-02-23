port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes as A exposing (style, type_)
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
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { playing = False
      , value = ""
      }
    , Cmd.none
    )


type Msg
    = PlayKick
    | PlaySequence
    | StopSequence
    | Slide String


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

        Slide value ->
            let
                floatValue =
                    parseString value
            in
            case floatValue of
                Just float ->
                    ( { model | value = value }, updateKick { freq = float, pitch = 10, wave = "sin", decay = 0.1, bite = 0.5 } )

                Nothing ->
                    ( { model | value = value }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        buttonStyles =
            [ style "padding" "4px 12px"
            , style "background" "#e8e7e2"
            , style "border-radius" "28px"
            , style "font-size" "0.8em"
            , style "border" "2px solid grey"
            , style "margin-right" "10px"
            ]
    in
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "font-family" "Helvetica, sans-serif"
        , style "line-height" "3.5em"
        , style "width" "50vw"
        , style "min-width" "350px"
        , style "margin" "auto"
        ]
        [ button
            (onClick PlayKick :: buttonStyles)
            [ text "KICKKKKK" ]
        , playingButton model.playing
        , input [ type_ "range", A.min "30", A.max "80", A.step "0.1", onInput Slide ] []
        , text model.value
        ]


playingButton : Bool -> Html Msg
playingButton isPlaying =
    let
        buttonStyles =
            [ style "padding" "4px 12px"
            , style "background" "#e8e7e2"
            , style "border-radius" "28px"
            , style "font-size" "0.8em"
            , style "border" "2px solid grey"
            , style "margin-right" "10px"
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
