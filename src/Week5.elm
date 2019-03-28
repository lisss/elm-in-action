module Week5 exposing (MyJsonDecoder(..), Path(..), Val(..), decodePath, decodeValue, keyValPairs, myJsonField, myJsonInt, myJsonString)

import Dict
import Json.Decode as J
import Json.Decode.Pipeline exposing (required)



-- Write your own json package


type Val
    = Str String
    | Num Int
    | Obj (List ( String, Val ))
    | Nil
    | Lst (List Val)


type MyJsonDecoder a
    = MyJsonDecoder (Val -> Result String a)



-- > decodeValue (myJsonInt) (Num 4)
-- Ok 4
-- > decodeValue (myJsonField "foo" myJsonInt) (Obj [("foo", Num 4)])
-- Ok 4


decodeValue : MyJsonDecoder a -> Val -> Result String a
decodeValue (MyJsonDecoder f) v =
    f v


myJsonString : MyJsonDecoder String
myJsonString =
    MyJsonDecoder
        (\v ->
            case v of
                Str r ->
                    Ok r

                _ ->
                    Err "Not str"
        )


myJsonInt : MyJsonDecoder Int
myJsonInt =
    MyJsonDecoder
        (\v ->
            case v of
                Num r ->
                    Ok r

                _ ->
                    Err "Not int"
        )


myJsonField : String -> MyJsonDecoder a -> MyJsonDecoder a
myJsonField k (MyJsonDecoder d) =
    MyJsonDecoder
        (\v ->
            case v of
                Obj i ->
                    i
                        |> Dict.fromList
                        |> Dict.get k
                        |> Result.fromMaybe "No such field"
                        |> Result.andThen d

                _ ->
                    Err "Not an object"
        )



-- > decodeValue (keyValPairs myJsonInt) (Obj [("alice", Num 42), ("bob", Num 99)])
-- Ok [("alice",42),("bob",99)]


keyValPairs : MyJsonDecoder a -> MyJsonDecoder (List ( String, a ))
keyValPairs (MyJsonDecoder d) =
    MyJsonDecoder
        (\v ->
            case v of
                Obj i ->
                    Ok
                        (i
                            |> List.filterMap
                                (\( x, y ) ->
                                    case d y of
                                        Ok res ->
                                            Just ( x, res )

                                        Err _ ->
                                            Nothing
                                )
                        )

                _ ->
                    Err "Not an object"
        )



-- Dependent parsing
-- > J.decodeString decodePath """{"path_type": "file", "value": "/foo/bar"}"""
-- Ok (PathFile "/foo/bar")
-- > J.decodeString decodePath """{"path_type": "url", "value": {"host": "http://example.com", "port_": 80}}"""
-- Ok (PathUrl { host = "http://example.com", port_ = 80 })


type Path
    = PathFile String
    | PathUrl { host : String, port_ : Int }


decodePath : J.Decoder Path
decodePath =
    J.oneOf
        [ J.succeed PathFile
            |> required "value" J.string
        , J.succeed PathUrl
            |> required "value"
                (J.map2
                    (\x y -> { host = x, port_ = y })
                    (J.field "host" J.string)
                    (J.field "port_" J.int)
                )
        ]



-- Date picker - see src/flights/ & dev:fs script in package.json for trying it out
