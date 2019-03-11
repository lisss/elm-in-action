module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import TemperatureConverter as TC
import Timer as T


type Msg
    = Converter TC.Msg
    | Timer T.Msg


type alias Model =
    { tempConverter : TC.Model
    , timer : T.Model
    }


initialModel : Model
initialModel =
    { tempConverter = TC.initialModel
    , timer = T.initialModel
    }


view : Model -> Html Msg
view model =
    div []
        [ div [] [ h1 [] [ text "Temperature Converter" ], Html.map Converter (TC.view model.tempConverter) ]
        , div [] [ h1 [] [ text "Timer" ], Html.map Timer (T.view model.timer) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Converter x ->
            ( { model | tempConverter = TC.update x model.tempConverter }, Cmd.none )

        Timer x ->
            let
                ( tModel, tMsg ) =
                    T.update x model.timer
            in
            ( { model | timer = tModel }, Cmd.map Timer tMsg )


mainCmd : Cmd Msg
mainCmd =
    Cmd.batch [ Cmd.map Timer T.getInitTime ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, mainCmd )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.map Timer (T.subscription model.timer)
        }
