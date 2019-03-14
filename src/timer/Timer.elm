module Timer exposing (Model, Msg(..), getInitTime, initialModel, subscription, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import Round exposing (round)
import Task
import Time


type Msg
    = GotInitialTime Time.Posix
    | TimeUpdate Time.Posix
    | Reset
    | Slide Float


type alias Model =
    { init : Float
    , elapsed : Float
    , duration : Float
    }


view : Model -> Html Msg
view model =
    div [ class "timer-container" ]
        [ div [ class "elapsed" ]
            [ span [ class "timer-label" ] [ text "Elapsed time:" ]
            , div [ class "progress-container" ]
                [ div
                    [ class "progressbar"
                    , ("%" |> (++) (model.elapsed / model.duration * 100 |> round 1))
                        |> style "width"
                    ]
                    []
                ]
            ]
        , span [ class "timer-label" ] [ text <| round 1 model.elapsed ++ "s" ]
        , div [ class "slider" ]
            [ span [ class "timer-label" ] [ text "Duration:" ]
            , input
                [ type_ "range"
                , class "slider-scale"
                , A.min "0"
                , A.max "20"
                , model.duration
                    |> String.fromFloat
                    |> value
                , String.toFloat >> Maybe.withDefault 0 >> Slide |> onInput
                ]
                []
            ]
        , button [ class "reset-btn", onClick Reset ] [ text "Reset" ]
        ]


initialModel : Model
initialModel =
    { init = 0
    , elapsed = 0
    , duration = 3
    }


timeToFloat : Time.Posix -> Float
timeToFloat =
    Time.posixToMillis >> toFloat


getElapsedTime : Model -> Time.Posix -> Float
getElapsedTime model t =
    let
        diff =
            (timeToFloat t - model.init) / 1000
    in
    if diff <= model.duration then
        diff

    else
        model.duration


getInitTime : Cmd Msg
getInitTime =
    Time.now
        |> Task.perform GotInitialTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate t ->
            ( { model | elapsed = getElapsedTime model t }
            , Cmd.none
            )

        Slide d ->
            ( { model | duration = d }, Cmd.none )

        Reset ->
            ( model, getInitTime )

        GotInitialTime t ->
            ( { model | elapsed = 0, init = timeToFloat t }, Cmd.none )


subscription : Model -> Sub Msg
subscription model =
    Time.every 200 TimeUpdate
