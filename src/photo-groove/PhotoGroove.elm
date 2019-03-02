module PhotoGroove exposing (main)    
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Browser
import Random

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize
            Loading -> []
            Errored errorMessage -> [ text ("Error: " ++ errorMessage) ]
        

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ]
            []
        ]

type Msg
    = ClickedPhoto String
    | GotRandomPhoto Photo
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

type alias Photo =
    { url : String }

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Model =
    { status: Status
    , chosenSize : ThumbnailSize
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url -> ( { model | status = selectUrl url model.status }, Cmd.none )
        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )
        ClickedSurpriseMe ->
            case model.status of
                Loaded [] _ -> ( model, Cmd.none )
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model
                    , Random.generate GotRandomPhoto
                        (Random.uniform firstPhoto otherPhotos)
                    )
                Loading ->
                    ( model, Cmd.none )
                Errored errorMessage ->
                    ( model, Cmd.none )
        ClickedSize size -> ( { model | chosenSize = size }, Cmd.none )

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored errorMessage ->
            status

initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }

type ThumbnailSize
    = Small
    | Medium
    | Large

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
    [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
    , text (sizeToString size)
    ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "med"
        Large -> "large"

main : Program () Model Msg
main = Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
