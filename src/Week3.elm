module Week3 exposing (either, find, keepOks, mapOk, maybeToList, updateList, updateListKv)

import Array exposing (..)
import Date exposing (Date)
import Html exposing (text)
import Maybe.Extra exposing (isJust)


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Nothing ->
            []

        Just y ->
            [ y ]



-- > updateList (\x -> x == 3) (\v -> Just (v + 1)) [1,3,5]
-- [1,4,5] : List number
-- > updateList (\x -> x == 3) (\v -> Nothing) [1,3,5]
-- [1,5] : List number


updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList f g =
    List.filterMap
        (\x ->
            if f x then
                g x

            else
                Just x
        )



-- > find (\x -> x == 2) [1,3,5,2]
-- Just 2 : Maybe number
-- > find (\x -> x == 2) [1,3,5]
-- Nothing : Maybe number


find : (a -> Bool) -> List a -> Maybe a
find f =
    List.foldr
        (\x y ->
            if f x then
                Just x

            else
                y
        )
        Nothing



-- > updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Just (x + 1))
-- [("foo", 2), ("bar", 2)]
-- > updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Nothing)
-- [("bar", 2)]
-- TODO: make shorter


updateListKv : List ( k, v ) -> k -> (v -> Maybe v) -> List ( k, v )
updateListKv xs key f =
    List.filterMap
        (\( k, v ) ->
            if k == key then
                case f v of
                    Just r ->
                        Just ( k, r )

                    Nothing ->
                        Nothing

            else
                Just ( k, v )
        )
        xs



-- > keepOks [Ok 1, Err "bad", Ok 2]
-- [1,2] : List number


keepOks : List (Result a b) -> List b
keepOks =
    Result.toMaybe |> List.filterMap



-- > mapOk (\x -> x + 1) (Ok 2)
-- Ok 3 : Result a number
-- > mapOk (\x -> x + 1) (Err "str")
-- Err "str" : Result String number


mapOk : (b -> c) -> Result a b -> Result a c
mapOk f x =
    case x of
        Ok r ->
            Ok (f r)

        Err e ->
            Err e


mapOk1 : (b -> c) -> Result a b -> Result a c
mapOk1 =
    Result.map



-- > either (\x -> x + 1) (\x -> x - 1) (Ok 1)
-- 0 : number
-- > either (\x -> x + 1) (\x -> x - 1) (Err 1)
-- 2 : number


either : (a -> c) -> (b -> c) -> Result a b -> c
either f g x =
    case x of
        Ok r ->
            g r

        Err e ->
            f e



-- Implement parseDate
-- > parseDate (Just "1970-01-01")
-- Just (RD 719163) : Maybe Date
-- > parseDate (Just "1970-01-")
-- Nothing : Maybe Date


parseDate : Maybe String -> Maybe Date
parseDate =
    Maybe.andThen <|
        Date.fromIsoString
            >> Result.toMaybe



-- Timer - see timer/Timer.elm
-- TODO: Graceful Labeling
