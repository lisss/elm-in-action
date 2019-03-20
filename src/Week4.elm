module Week4 exposing (User, decodeUser)

import Dict
import Iso8601 as ISO
import Json.Decode as J
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Json.Encode.Extra as EncodeEx
import Time



-- Decode User
-- > J.decodeString decodeUser "{\"name\": \"Kostia\", \"cats\": 2}"
-- Ok { cats = Just 2, name = "Kostia" }
--     : Result J.Error User
-- > J.decodeString decodeUser "{\"name\": \"Droopy The Dog\", \"cats\": null}"
-- Ok { cats = Nothing, name = "Droopy The Dog" }
--     : Result J.Error User


type alias User =
    { name : String
    , cats : Maybe Int
    }


decodeUser : J.Decoder User
decodeUser =
    J.succeed User
        |> required "name" J.string
        |> optional "cats" (J.map Just J.int) Nothing



-- Mottos
-- J.decodeString decodeMottos mottos
-- Ok { countries = [
-- { currency = "EUR", motto = "Einigkeit und Recht und Freiheit", name = "Germany" }
-- ,{ currency = "GBP", motto = "God Save the Queen", name = "England" }
-- ,{ currency = "EUR", motto = "Liberté, Égalité, Fraternité", name = "France" }
-- ] }


mottos : String
mottos =
    """
      {"Germany": {"motto": "Einigkeit und Recht und Freiheit", "currency": "EUR"},
       "England": {"motto": "God Save the Queen", "currency": "GBP"},
       "France": {"motto": "Liberté, Égalité, Fraternité", "currency": "EUR"}}
      """


type alias Mottos =
    { countries : List Country }


type alias Country =
    { name : String
    , motto : String
    , currency : String
    }


type alias Info =
    { motto : String
    , currency : String
    }


countryDecoder : J.Decoder Country
countryDecoder =
    J.succeed Country
        |> hardcoded ""
        |> required "motto" J.string
        |> required "currency" J.string


decodeMottos : J.Decoder Mottos
decodeMottos =
    J.keyValuePairs countryDecoder
        |> J.map (\x -> Mottos (List.map (\( name, country ) -> { country | name = name }) x))



-- Date decoder
-- > J.decodeString decodeDate "\"2018-10-01T12:48:00.000Z\""
-- Ok (Posix 1538398080000)
--   : Result J.Error Time.Posix
-- TODO: make it manually


decodeDate : J.Decoder Time.Posix
decodeDate =
    ISO.decoder



-- Account info
-- J.decodeString jsonDecAccountInfo infoComplete
-- > Ok { email = "liss@test.com", full_name = Just "Liss", id = 666, info_complete = True, phone_number = Just "666" }
-- J.decodeString jsonDecAccountInfo infoIncomplete
-- > Ok { email = "liss@test.com", full_name = Nothing, id = 666, info_complete = False, phone_number = Nothing }
-- > jsonEncAccountInfo accComplete |> Encode.encode 0
-- "{\"id\":777,\"email\":\"liss@test.com\",\"full_name\":\"Liss\",\"phone_number\":\"888\",\"info_complete\":true}"
--     : String
-- > jsonEncAccountInfo accIncomplete |> Encode.encode 0
-- "{\"id\":777,\"email\":\"liss@test.com\",\"full_name\":null,\"phone_number\":null,\"info_complete\":true}"
--     : String


infoComplete : String
infoComplete =
    """
    {"id": 666, "email": "liss@test.com", "full_name": "Liss", "phone_number": "666", "info_complete": true}
    """


infoIncomplete : String
infoIncomplete =
    """
    {"id": 666, "email": "liss@test.com", "full_name": null, "phone_number": null, "info_complete": false}
    """


type alias AccountInfo =
    { id : Int
    , email : String
    , full_name : Maybe String
    , phone_number : Maybe String
    , info_complete : Bool
    }


jsonDecAccountInfo : J.Decoder AccountInfo
jsonDecAccountInfo =
    J.succeed AccountInfo
        |> required "id" J.int
        |> required "email" J.string
        |> optional "full_name" (J.map Just J.string) Nothing
        |> optional "phone_number" (J.map Just J.string) Nothing
        |> required "info_complete" J.bool


accComplete : AccountInfo
accComplete =
    AccountInfo 777 "liss@test.com" (Just "Liss") (Just "888") True


accIncomplete : AccountInfo
accIncomplete =
    AccountInfo 777 "liss@test.com" Nothing Nothing True


jsonEncAccountInfo : AccountInfo -> J.Value
jsonEncAccountInfo { id, email, full_name, phone_number, info_complete } =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "email", Encode.string email )
        , ( "full_name", EncodeEx.maybe Encode.string full_name )
        , ( "phone_number", EncodeEx.maybe Encode.string phone_number )
        , ( "info_complete", Encode.bool info_complete )
        ]



-- jsonPair
-- > J.decodeString (jsonPair J.string J.string) "[\"Louee\", \"Baton\"]"
-- Ok ("Louee","Baton")
--     : Result J.Error ( String, String )
-- > J.decodeString (jsonPair J.string J.string) "[\"Louee\"]"
-- Err (Failure ("Expecting a list of two elements") <internals>)
--     : Result J.Error ( String, String )
-- TODO: achieve the same error


jsonPair : J.Decoder a -> J.Decoder b -> J.Decoder ( a, b )
jsonPair x y =
    J.map2 Tuple.pair (J.index 0 x) (J.index 1 y)



-- CRUD - see crud/Crud.elm
-- Morse Code - see MorseCode.elm
