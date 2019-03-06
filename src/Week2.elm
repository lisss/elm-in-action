module Week2 exposing (Address, Contact, Profile, User, bird, bird2, bird3, buildStatsUrl, catMaybes, catMaybes02, catMaybes03, convert, convert02, convert03, mapMaybes, mapMaybes02, setPhone02)

import Array
import List exposing (filter, length, map)
import Random
import Url.Builder exposing (absolute, int, string)


type alias Contact =
    { name : String
    , email : String
    }



{-
   data = [
      { name = Just "liss1", email = Just "l1@a.b" }
     ,{ name = Just "liss2", email = Just "l2@a.b" }
     ,{ name = Just "liss3", email = Nothing }
     ,{ name = Nothing, email = Just "l3@a.b" }
     ]
-}
-- Map one structure to another


convert :
    List { name : String, email : String, phone_number : String }
    -> List { name : String, email : String }
convert =
    map
        (\{ name, email } -> Contact name email)



-- Filter elements with non-empty name and email


convert02 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert02 =
    map
        (\{ name, email } ->
            Contact (Maybe.withDefault "" name) (Maybe.withDefault "" email)
        )
        >> filter (\{ name, email } -> name /= "" && email /= "")



-- Fill in missing emails with <unspecified>, while removing elements with no name


convert03 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert03 =
    map
        (\{ name, email } ->
            Contact (Maybe.withDefault "" name) (Maybe.withDefault "<unspecified>" email)
        )
        >> filter (\{ name } -> name /= "")



-- Rewrite bird using <|, then using |> instead of parens (where applicable)


bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))



-- using <|


bird2 : Int
bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr [ 1, 2, 3 ]



-- using |>


bird3 : Int
bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ]
        |> List.map incr
        |> List.filter notThree
        |> List.sum



-- Implement setPhone
-- > setPhone "+123456" { profile = { address = { phone = "+654321" } } }
-- { profile = { address = { phone = "+123456" } } }


type alias User =
    { profile : Profile }


type alias Address =
    { phone : String }


type alias Profile =
    { address : Address }


setPhone : String -> User -> User
setPhone p u =
    let
        { profile } =
            u

        { address } =
            profile
    in
    { u | profile = { profile | address = { address | phone = p } } }



-- Simple case (if record types won't change)


setPhone02 : String -> User -> User
setPhone02 p u =
    { u | profile = { address = { phone = p } } }



-- > mapMaybes (\x -> if x == Just 3 then x else Just 4) [Just 1, Nothing, Just 3]
-- [4,4,3] : List number
-- It's actually List.filterMap


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes f xss =
    case xss of
        [] ->
            []

        x :: xs ->
            case f x of
                Just r ->
                    r :: mapMaybes f xs

                Nothing ->
                    mapMaybes f xs


mapMaybes02 f xss =
    case xss of
        [] ->
            []

        xs ->
            catMaybes xs



-- > catMaybes [Just 1, Nothing, Just 3]
-- [1,3] : List number


catMaybes : List (Maybe a) -> List a
catMaybes xss =
    case xss of
        [] ->
            []

        x :: xs ->
            case x of
                Just r ->
                    r :: catMaybes xs

                Nothing ->
                    catMaybes xs


catMaybes02 : List (Maybe a) -> List a
catMaybes02 =
    List.filterMap identity


catMaybes03 : List (Maybe a) -> List a
catMaybes03 =
    mapMaybes identity



-- > buildStatsUrl 12 { startDate = Nothing, numElems = Nothing }
-- https://myapi.com/api/item/12/stats.json
-- > buildStatsUrl 12 { startDate = Just "2019-01-01", numElems = Nothing }
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01
-- > buildStatsUrl 12 { startDate = Just "2019-01-01", numElems = Just 10 }
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01&num_items=10


buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
    let
        { startDate, numElems } =
            ps

        qps =
            [ Maybe.andThen (Just << string "start_date") startDate
            , Maybe.andThen (Just << int "num_items") numElems
            ]
                |> catMaybes
    in
    "https://myapi.com/api/item"
        ++ absolute [ String.fromInt itemId ++ "/stats.json" ] qps



{- Temperature converter
   check ./temp-converter/TemperatureConverter.elm & package.json to run it
-}
-- Eight Queens
-- Honestly stolen from somewhere on the Internet


queens =
    addQueen 7 8


addQueen : Int -> Int -> List (List Int)
addQueen newRow cols =
    let
        queenPuzzle r c =
            if r <= 0 then
                [ [] ]

            else
                addQueen (r - 1) c

        prev =
            queenPuzzle newRow cols
    in
    List.concatMap
        (\solution ->
            List.filterMap
                (\newCol ->
                    if not (hasConflict newRow newCol solution) then
                        Just (solution ++ [ newCol ])

                    else
                        Nothing
                )
                (List.range 1 cols)
        )
        prev


hasConflict : Int -> Int -> List Int -> Bool
hasConflict newRow newCol solution =
    List.foldl
        (\i conflict ->
            let
                ithSolution =
                    case Array.get i (Array.fromList solution) of
                        Just value ->
                            value

                        Nothing ->
                            0
            in
            conflict
                || (ithSolution == newCol)
                || (ithSolution + i == newCol + newRow)
                || (ithSolution - i == newCol - newRow)
        )
        False
        (List.range 0 newRow)
