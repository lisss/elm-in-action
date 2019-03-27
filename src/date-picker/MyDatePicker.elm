module MyDatePicker exposing (Carrier, Currency, FlightDetails, Leg, Model, Msg(..), Place, Place1, Quote, Res, apiKey, apiUrl, carriersDecoder, getToday, init, legDecoder, main, placesDecoder, quotePlaceDecoder, quotesDecoder, searchPlaces, searchResultsDecoder, searchRoutes, settings, toRes, update, view)

import Array exposing (Array)
import Browser
import Date exposing (Date, weekday)
import DatePicker exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder, bool, dict, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Task
import Time exposing (Weekday(..))


apiKey =
    "d91d75e6dfmsha945160721f4570p1e7144jsn7f1ada7e99be"


apiUrl =
    "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/"


type Search
    = From
    | To


type Msg
    = SetDatePickerStart DatePicker.Msg
    | SetDatePickerEnd DatePicker.Msg
    | GotToday Date
    | SearchPlaces Search String
    | SearchRoutes String String
    | GotPlacesFrom (Result Http.Error (List Place))
    | GotPlacesTo (Result Http.Error (List Place))
    | PlaceFrom String
    | PlaceTo String
    | GotSearchResults (Result Http.Error (List Res))


type alias Model =
    { today : Maybe Date
    , dateStart : Maybe Date
    , dateEnd : Maybe Date
    , datePickerStart : DatePicker.DatePicker
    , datePickerEnd : DatePicker.DatePicker
    , dateRangeSelected : Maybe (List Date)
    , searchPlacesFrom : List Place
    , searchPlacesTo : List Place
    , searchFrom : String
    , searchTo : String
    , searchResults : List Res
    }


settings : Model -> DatePicker.Settings
settings model =
    let
        isDisabled date =
            case model.today of
                Nothing ->
                    False

                Just x ->
                    Date.compare date x == LT
    in
    { defaultSettings | firstDayOfWeek = Mon, isDisabled = isDisabled }


view : Model -> Html Msg
view model =
    div [ class "dp-main-container" ]
        [ div [ class "dp-wrapper" ]
            [ div
                []
                [ text "Depart date" ]
            , DatePicker.view
                model.dateStart
                (settings
                    model
                )
                model.datePickerStart
                |> Html.map SetDatePickerStart
            ]
        , div [ class "dp-wrapper" ]
            [ div
                []
                [ text "Return date" ]
            , DatePicker.view
                model.dateEnd
                (settings
                    model
                )
                model.datePickerEnd
                |> Html.map SetDatePickerEnd
            ]
        , div []
            [ label []
                [ text "From" ]
            , input
                [ onInput <| SearchPlaces From ]
                []
            , ul [ class "from" ] <|
                List.map
                    (\p ->
                        li
                            [ class <|
                                if p.placeId == model.searchFrom then
                                    "search-item active"

                                else
                                    "search-item"
                            , onClick <|
                                PlaceFrom p.placeId
                            ]
                            [ text <| p.placeName ++ ", " ++ p.countryName ]
                    )
                    model.searchPlacesFrom
            ]
        , div []
            [ label []
                [ text "To" ]
            , input
                [ onInput <| SearchPlaces To ]
                []
            , ul [ class "to" ] <|
                List.map
                    (\p ->
                        li
                            [ class <|
                                if p.placeId == model.searchTo then
                                    "search-item active"

                                else
                                    "search-item"
                            , onClick <| PlaceTo p.placeId
                            ]
                            [ text <| p.placeName ++ ", " ++ p.countryName ]
                    )
                    model.searchPlacesTo
            ]
        , button [ onClick <| SearchRoutes model.searchFrom model.searchTo ] [ text "Search routes" ]
        , div [] <|
            List.map
                (\p ->
                    div []
                        [ text <| "Min price:" ++ p.currency.symbol ++ String.fromInt p.minPrice
                        , div []
                            [ text <|
                                p.outb.origin.name
                                    ++ " -> "
                                    ++ p.outb.destination.name
                            , div [] <| List.map (\c -> text c.name) p.outb.carriers
                            ]
                        , div []
                            [ text <|
                                p.inb.origin.name
                                    ++ " -> "
                                    ++ p.inb.destination.name
                            , div [] <| List.map (\c -> text c.name) p.inb.carriers
                            ]
                        ]
                )
                model.searchResults
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDatePickerStart subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (settings model) subMsg model.datePickerStart

                date =
                    case dateEvent of
                        Picked newDate ->
                            Just newDate

                        _ ->
                            model.dateStart

                d =
                    Debug.log ">> date1" date
            in
            ( { model
                | dateStart = date
                , datePickerStart = newDatePicker
              }
            , Cmd.none
            )

        SetDatePickerEnd subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (settings model) subMsg model.datePickerEnd

                date =
                    case dateEvent of
                        Picked newDate ->
                            Just newDate

                        _ ->
                            model.dateEnd

                d =
                    Debug.log ">> date2" date
            in
            ( { model
                | dateEnd = date
                , datePickerEnd = newDatePicker
              }
            , Cmd.none
            )

        GotToday day ->
            let
                d =
                    Debug.log "date" <| Date.toIsoString day
            in
            ( { model | today = Just day }, Cmd.none )

        SearchPlaces t p ->
            ( model, searchPlaces t p )

        SearchRoutes f t ->
            case
                Maybe.map2 (\s e -> ( model, searchRoutes f t (Date.toIsoString s) (Date.toIsoString e) ))
                    model.dateStart
                    model.dateEnd
            of
                Nothing ->
                    ( model, Cmd.none )

                Just c ->
                    c

        GotPlacesFrom res ->
            case res of
                Ok places ->
                    ( { model | searchPlacesFrom = places }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotPlacesTo res ->
            case res of
                Ok places ->
                    ( { model | searchPlacesTo = places }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PlaceFrom p ->
            ( { model | searchFrom = p }, Cmd.none )

        PlaceTo p ->
            ( { model | searchTo = p }, Cmd.none )

        GotSearchResults res ->
            case res of
                Ok flights ->
                    ( { model | searchResults = flights }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    let
        ( datePickerStart, datePickerCmdStart ) =
            DatePicker.init

        ( datePickerEnd, datePickerCmdEnd ) =
            DatePicker.init
    in
    ( { today = Nothing
      , dateStart = Nothing
      , dateEnd = Nothing
      , datePickerStart = datePickerStart
      , datePickerEnd = datePickerEnd
      , dateRangeSelected = Nothing
      , searchPlacesFrom = []
      , searchPlacesTo = []
      , searchFrom = ""
      , searchTo = ""
      , searchResults = []
      }
    , Cmd.batch
        [ Cmd.map SetDatePickerStart datePickerCmdStart
        , Cmd.map SetDatePickerEnd datePickerCmdEnd
        , getToday
        ]
    )


type alias Place =
    { placeId : String
    , placeName : String
    , countryId : String
    , regionId : String
    , countryName : String
    }


type alias Carrier =
    { carrierId : Int
    , name : String
    }


type alias Currency =
    { code : String
    , symbol : String
    }


type alias Place1 =
    { cityName : String
    , countryName : String
    , iataCode : String
    , name : String
    , placeId : Int
    }


type alias Leg =
    { carrierIds : List Int
    , destinationId : Int
    , originId : Int
    }


type alias Quote =
    { direct : Bool
    , minPrice : Int
    , outboundLeg :
        Leg
    , inboundLeg :
        Leg
    }


legDecoder : Decoder Leg
legDecoder =
    D.succeed Leg
        |> required "CarrierIds" (list int)
        |> required "OriginId" int
        |> required "DestinationId" int


quotesDecoder : Decoder (List Quote)
quotesDecoder =
    D.field "Quotes"
        (D.list
            (D.succeed Quote
                |> required "Direct" bool
                |> required "MinPrice" int
                |> required "OutboundLeg" legDecoder
                |> required "InboundLeg" legDecoder
            )
        )


carriersDecoder : Decoder (Dict Int Carrier)
carriersDecoder =
    D.field "Carriers"
        (D.list
            (D.succeed Carrier
                |> required "CarrierId" int
                |> required "Name" string
            )
        )
        |> D.map
            (\x ->
                x
                    |> List.map (\c -> ( c.carrierId, c ))
                    |> Dict.fromList
            )


quotePlaceDecoder : Decoder (Dict Int Place1)
quotePlaceDecoder =
    D.field "Places"
        (D.list
            (D.succeed Place1
                |> required "CityName" string
                |> required "CountryName" string
                |> required "IataCode" string
                |> required "Name" string
                |> required "PlaceId" int
            )
            |> D.map
                (\x ->
                    x
                        |> List.map (\p -> ( p.placeId, p ))
                        |> Dict.fromList
                )
        )


curenciesDecoder : Decoder (List Currency)
curenciesDecoder =
    D.field "Currencies"
        (D.list
            (D.succeed Currency
                |> required "Code" string
                |> required "Symbol" string
            )
        )


placesDecoder : Decoder (List Place)
placesDecoder =
    D.field "Places"
        (D.list
            (D.succeed Place
                |> required "PlaceId" string
                |> required "PlaceName" string
                |> required "CountryId" string
                |> required "RegionId" string
                |> required "CountryName" string
            )
        )


type alias Route =
    { name : String
    , country : String
    }


type alias FlightDetails =
    { origin : Route
    , destination : Route
    , carriers : List Carrier
    }



--  FIXMEK


type alias Res =
    { outb : FlightDetails
    , inb : FlightDetails
    , minPrice : Int
    , currency : Currency
    }


getFlight : Quote -> Dict Int Place1 -> Dict Int Carrier -> List Currency -> Maybe Res
getFlight q pl car curr =
    Maybe.map4
        (\org dst orgi dsti ->
            Res
                { origin = { name = org.name, country = org.countryName }
                , destination = { name = dst.name, country = dst.countryName }
                , carriers = List.filterMap (\x -> Dict.get x car) q.outboundLeg.carrierIds
                }
                { origin = { name = orgi.name, country = orgi.countryName }
                , destination = { name = dsti.name, country = dsti.countryName }
                , carriers = List.filterMap (\x -> Dict.get x car) q.inboundLeg.carrierIds
                }
                q.minPrice
                (Maybe.withDefault
                    (Currency
                        "EUR"
                        "€"
                    )
                    (List.head
                        curr
                    )
                )
        )
        (Dict.get
            q.inboundLeg.originId
            pl
        )
        (Dict.get
            q.inboundLeg.destinationId
            pl
        )
        (Dict.get
            q.outboundLeg.originId
            pl
        )
        (Dict.get
            q.outboundLeg.destinationId
            pl
        )


toRes : Dict Int Carrier -> Dict Int Place1 -> List Quote -> List Currency -> List Res
toRes c p q cu =
    List.filterMap
        (\qx ->
            getFlight qx p c cu
        )
        q


searchResultsDecoder : Decoder (List Res)
searchResultsDecoder =
    D.map4 toRes carriersDecoder quotePlaceDecoder quotesDecoder curenciesDecoder


searchPlaces : Search -> String -> Cmd Msg
searchPlaces t query =
    Http.request
        { method = "GET"
        , url = apiUrl ++ "autosuggest/v1.0/UA/EUR/en-US/?query=" ++ query
        , headers = [ Http.header "X-RapidAPI-Key" apiKey ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (case t of
                    From ->
                        GotPlacesFrom

                    To ->
                        GotPlacesTo
                )
                placesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


searchRoutes : String -> String -> String -> String -> Cmd Msg
searchRoutes f t s e =
    Http.request
        { method = "GET"
        , url = apiUrl ++ "browsequotes/v1.0/US/EUR/en-US/" ++ f ++ "/" ++ t ++ "/" ++ s ++ "/" ++ e
        , headers = [ Http.header "X-RapidAPI-Key" apiKey ]
        , body = Http.emptyBody
        , expect = Http.expectJson GotSearchResults searchResultsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getToday : Cmd Msg
getToday =
    Date.today
        |> Task.perform GotToday


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
