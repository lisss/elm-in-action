module Main exposing (main)

import Browser
import Crud as Cr
import Html exposing (..)
import Html.Attributes exposing (..)
import TemperatureConverter as TC
import Timer as T


type Msg
    = Converter TC.Msg
    | Timer T.Msg
    | Crud Cr.Msg


type alias Model =
    { tempConverter : TC.Model
    , timer : T.Model
    , crud : Cr.Model
    }


initialModel : Model
initialModel =
    { tempConverter = TC.initialModel
    , timer = T.initialModel
    , crud = Cr.initialModel
    }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "section" ] [ h1 [] [ text "Temperature Converter" ], Html.map Converter (TC.view model.tempConverter) ]
        , div [ class "section" ] [ h1 [] [ text "Timer" ], Html.map Timer (T.view model.timer) ]
        , div [ class "section" ] [ h1 [] [ text "CRUD" ], Html.map Crud (Cr.view model.crud) ]
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

        Crud x ->
            let
                ( cModel, cMsg ) =
                    Cr.update x model.crud
            in
            ( { model | crud = cModel }, Cmd.map Crud cMsg )


mainCmd : Cmd Msg
mainCmd =
    Cmd.batch [ Cmd.map Timer T.getInitTime, Cmd.map Crud Cr.initialCmd ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, mainCmd )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.map Timer (T.subscription model.timer)
        }
