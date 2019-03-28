module FlightSearch exposing (Carrier, Currency, FlightDetails, Leg, Model, Msg(..), Place, Quote, apiKey, apiUrl, carriersDecoder, getToday, init, legDecoder, main, quotePlaceDecoder, quotesDecoder, searchFlights, searchResultsDecoder, settings, update, view)

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
import Maybe.Extra exposing (isNothing)
import Task
import Time exposing (Weekday(..))


apiKey =
    "d91d75e6dfmsha945160721f4570p1e7144jsn7f1ada7e99be"


apiUrl =
    "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/"


type alias Suggestion =
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


type alias Place =
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


type Search
    = From
    | To


type State
    = Success
    | Loading
    | Error String


type alias PlaceLight =
    { key : String
    , name : String
    }


type Msg
    = SetDatePickerStart DatePicker.Msg
    | SetDatePickerEnd DatePicker.Msg
    | GotToday Date
    | SearchPlaces Search String
    | SearchRoutes String String
    | GotSuggestionsFrom (Result Http.Error (List Suggestion))
    | GotSuggestionsTo (Result Http.Error (List Suggestion))
    | Origin PlaceLight
    | Destination PlaceLight
    | GotSearchResults (Result Http.Error (List SearchResult))


type alias Model =
    { today : Maybe Date
    , dateStart : Maybe Date
    , dateEnd : Maybe Date
    , datePickerStart : DatePicker.DatePicker
    , datePickerEnd : DatePicker.DatePicker
    , dateRangeSelected : Maybe (List Date)
    , suggestionsFrom : List Suggestion
    , suggestionsTo : List Suggestion
    , origin : PlaceLight
    , destination : PlaceLight
    , searchResults : List SearchResult
    , state : State
    , originSearchValue : String
    , destSearchValue : String
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


viewSearchPlace : String -> Search -> String -> (PlaceLight -> Msg) -> List Suggestion -> Bool -> String -> Html Msg
viewSearchPlace label search dir msg sgs closed val =
    div [ class "fs-wrapper fs-searh-wrapper" ]
        [ div [ class "fs-label" ]
            [ text label ]
        , input
            [ onInput <| SearchPlaces search
            , value val
            ]
            []
        , ul
            [ class <|
                if closed == True then
                    "fs-search-list closed"

                else
                    "fs-search-list"
            ]
          <|
            List.map
                (\p ->
                    li
                        [ class <|
                            if p.placeId == dir then
                                "search-item active"

                            else
                                "search-item"
                        , onClick <|
                            msg <|
                                PlaceLight p.placeId p.placeName
                        ]
                        [ text <| p.placeName ++ ", " ++ p.countryName ]
                )
                sgs
        ]


viewSearchPlaces : Model -> Html Msg
viewSearchPlaces model =
    div [ class "fs-search" ]
        [ viewSearchPlace "From"
            From
            model.origin.key
            Origin
            model.suggestionsFrom
            (model.origin.name == model.originSearchValue || List.isEmpty model.suggestionsFrom)
            model.originSearchValue
        , viewSearchPlace "To"
            To
            model.destination.key
            Destination
            model.suggestionsTo
            (model.destination.name == model.destSearchValue || List.isEmpty model.suggestionsTo)
            model.destSearchValue
        ]


viewFlightDetails : String -> String -> List Carrier -> Html Msg
viewFlightDetails oName dName carrs =
    div [ class "fs-flight-details" ]
        [ div [ class "fs-flight-route" ]
            [ text oName
            , i [ class "fas fa-long-arrow-alt-right fs-arrow" ] []
            , text dName
            ]
        , div [ class "fs-carrier-wrap" ] <| List.map (\c -> div [] [ text c.name ]) carrs
        ]


viewSearchResults : Model -> Html Msg
viewSearchResults model =
    div [ class "fs-result-list" ] <|
        case model.state of
            Error e ->
                [ div [] [ text e ] ]

            -- TODO: add loader
            Loading ->
                [ text "" ]

            Success ->
                if List.isEmpty model.searchResults then
                    [ div [] [ text "No flights found :(" ] ]

                else
                    List.map
                        (\p ->
                            div [ class "fs-result" ]
                                [ div [ class "fs-result-price-route" ]
                                    [ div [ class "fs-route" ]
                                        [ text <|
                                            if p.direct == True then
                                                "Direct"

                                            else
                                                ""
                                        ]
                                    , div [ class "fs-price" ] [ text <| p.currency.symbol ++ String.fromInt p.minPrice ]
                                    ]
                                , div []
                                    [ viewFlightDetails p.outb.origin.name p.outb.destination.name p.outb.carriers
                                    , viewFlightDetails p.inb.origin.name p.inb.destination.name p.inb.carriers
                                    ]
                                ]
                        )
                        model.searchResults


view : Model -> Html Msg
view model =
    div [ class "fs-main-container" ]
        [ div [ class "fs-content" ]
            [ viewSearchPlaces model
            , div [ class "fs-wrapper" ]
                [ div
                    [ class "fs-label" ]
                    [ text "Depart date" ]
                , DatePicker.view
                    model.dateStart
                    (settings
                        model
                    )
                    model.datePickerStart
                    |> Html.map SetDatePickerStart
                ]
            , div [ class "fs-wrapper" ]
                [ div
                    [ class "fs-label" ]
                    [ text "Return date" ]
                , DatePicker.view
                    model.dateEnd
                    (settings
                        model
                    )
                    model.datePickerEnd
                    |> Html.map SetDatePickerEnd
                ]
            ]
        , div [ class "fs-search-wrapper" ]
            [ button
                [ class "fs-search-btn"
                , onClick <| SearchRoutes model.origin.key model.destination.key
                , disabled <|
                    String.isEmpty model.origin.key
                        || String.isEmpty model.destination.key
                        || isNothing model.dateStart
                        || isNothing model.dateEnd
                ]
                [ text "Search flights" ]
            , viewSearchResults model
            ]
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
            in
            ( { model
                | dateEnd = date
                , datePickerEnd = newDatePicker
              }
            , Cmd.none
            )

        GotToday day ->
            ( { model | today = Just day }, Cmd.none )

        SearchPlaces t p ->
            let
                m =
                    case t of
                        From ->
                            { model | originSearchValue = p }

                        To ->
                            { model | destSearchValue = p }
            in
            ( m, getSuggestions t p )

        SearchRoutes f t ->
            case
                Maybe.map2 (\s e -> ( model, searchFlights f t (Date.toIsoString s) (Date.toIsoString e) ))
                    model.dateStart
                    model.dateEnd
            of
                Nothing ->
                    ( model, Cmd.none )

                Just c ->
                    c

        GotSuggestionsFrom res ->
            case res of
                Ok places ->
                    ( { model | suggestionsFrom = places }, Cmd.none )

                Err _ ->
                    ( { model | suggestionsFrom = [] }, Cmd.none )

        GotSuggestionsTo res ->
            case res of
                Ok places ->
                    ( { model | suggestionsTo = places }, Cmd.none )

                Err _ ->
                    ( { model | suggestionsTo = [] }, Cmd.none )

        Origin p ->
            ( { model | origin = p, originSearchValue = p.name }, Cmd.none )

        Destination p ->
            ( { model | destination = p, destSearchValue = p.name }, Cmd.none )

        GotSearchResults res ->
            case res of
                Ok flights ->
                    ( { model | searchResults = flights, state = Success }, Cmd.none )

                Err _ ->
                    ( { model | state = Error "An error occured" }, Cmd.none )


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
      , suggestionsFrom = []
      , suggestionsTo = []
      , origin = PlaceLight "" ""
      , destination = PlaceLight "" ""
      , searchResults = []
      , state = Loading
      , originSearchValue = ""
      , destSearchValue = ""
      }
    , Cmd.batch
        [ Cmd.map SetDatePickerStart datePickerCmdStart
        , Cmd.map SetDatePickerEnd datePickerCmdEnd
        , getToday
        ]
    )


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


transformDecoder : (a -> comparable) -> Decoder (List a) -> Decoder (Dict comparable a)
transformDecoder f =
    D.map
        (\x ->
            x
                |> List.map (\r -> ( f r, r ))
                |> Dict.fromList
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
        |> transformDecoder .carrierId


quotePlaceDecoder : Decoder (Dict Int Place)
quotePlaceDecoder =
    D.field "Places"
        (D.list
            (D.succeed Place
                |> required "CityName" string
                |> required "CountryName" string
                |> required "IataCode" string
                |> required "Name" string
                |> required "PlaceId" int
            )
            |> transformDecoder .placeId
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


suggestionsDecoder : Decoder (List Suggestion)
suggestionsDecoder =
    D.field "Places"
        (D.list
            (D.succeed Suggestion
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


type alias SearchResult =
    { outb : FlightDetails
    , inb : FlightDetails
    , direct : Bool
    , minPrice : Int
    , currency : Currency
    }


getFlightDetails : Place -> Place -> Dict Int Carrier -> List Int -> FlightDetails
getFlightDetails orig dest car cars =
    { origin = { name = orig.name, country = orig.countryName }
    , destination = { name = dest.name, country = dest.countryName }
    , carriers = List.filterMap (\x -> Dict.get x car) cars
    }


getFlight : Quote -> Dict Int Place -> Dict Int Carrier -> List Currency -> Maybe SearchResult
getFlight q pl car curr =
    Maybe.map4
        (\iOrig iDst oOrig oDst ->
            SearchResult
                (getFlightDetails iOrig iDst car q.inboundLeg.carrierIds)
                (getFlightDetails oOrig oDst car q.outboundLeg.carrierIds)
                q.direct
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


searchResultsDecoder : Decoder (List SearchResult)
searchResultsDecoder =
    D.map4
        (\c p cu ->
            List.filterMap
                (\qx ->
                    getFlight qx p c cu
                )
        )
        carriersDecoder
        quotePlaceDecoder
        curenciesDecoder
        quotesDecoder


getSuggestions : Search -> String -> Cmd Msg
getSuggestions t query =
    Http.request
        { method = "GET"
        , url = apiUrl ++ "autosuggest/v1.0/UA/EUR/en-US/?query=" ++ query
        , headers = [ Http.header "X-RapidAPI-Key" apiKey ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (case t of
                    From ->
                        GotSuggestionsFrom

                    To ->
                        GotSuggestionsTo
                )
                suggestionsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


searchFlights : String -> String -> String -> String -> Cmd Msg
searchFlights f t s e =
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
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
