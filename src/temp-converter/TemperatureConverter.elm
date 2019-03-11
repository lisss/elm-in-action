module TemperatureConverter exposing (Model, Msg(..), initialModel, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type Msg
    = TempCUpdated String
    | TempFUpdated String


type alias Model =
    { tempC : String
    , tempF : String
    }


toCelcius : Float -> Float
toCelcius x =
    (x - 32) * (5 / 9)


toFahrenheit : Float -> Float
toFahrenheit x =
    x * (9 / 5) + 32


convert : (Float -> Float) -> String -> String
convert f s =
    case String.toFloat s of
        Nothing ->
            ""

        Just x ->
            String.fromFloat <| f x


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ input [ value model.tempC, onInput TempCUpdated ] []
        , span [ class "label" ] [ text "Celcius =" ]
        , input [ value model.tempF, onInput TempFUpdated ] []
        , span [ class "label" ] [ text "Fahrenheit" ]
        ]


initialModel : Model
initialModel =
    { tempC = ""
    , tempF = ""
    }


updateTemperature : String -> Model -> (String -> Model) -> Model
updateTemperature val model updateModel =
    if String.isEmpty val then
        initialModel

    else
        case String.toFloat val of
            Nothing ->
                model

            Just x ->
                updateModel <| String.fromFloat x


update : Msg -> Model -> Model
update msg model =
    case msg of
        TempCUpdated t ->
            updateTemperature t model (\x -> { model | tempC = x, tempF = convert toFahrenheit x })

        TempFUpdated t ->
            updateTemperature t model (\x -> { model | tempF = x, tempC = convert toCelcius x })
