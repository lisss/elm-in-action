module Week1 exposing (clap, compress, dropEvery, elementAt, isPalindrome, myButLast, myLast, myLength, myLength02, myReverse)

import List exposing (..)



-- foldl (Just >> always) Nothing


myLast : List a -> Maybe a
myLast l =
    case l of
        [] ->
            Nothing

        xs ->
            drop (length xs - 1) xs |> head


myButLast : List a -> Maybe a
myButLast l =
    case l of
        [] ->
            Nothing

        xs ->
            drop (length xs - 2) xs |> head


elementAt : List a -> Int -> Maybe a
elementAt l n =
    if n < 1 then
        Nothing

    else
        case l of
            [] ->
                Nothing

            xs ->
                drop (n - 1) xs |> head


myLength : List a -> Int
myLength l =
    case l of
        [] ->
            0

        x :: xs ->
            1 + myLength xs


myLength02 : List a -> Int
myLength02 =
    List.foldl (\_ acc -> 1 + acc) 0


myReverse : List a -> List a
myReverse l =
    case l of
        [] ->
            []

        x :: xs ->
            myReverse xs ++ [ x ]


isPalindrome : List a -> Bool
isPalindrome =
    \x -> x == reverse x


compress : String -> String
compress s =
    String.toList s
        |> (\l ->
                let
                    do ll =
                        case ll of
                            [] ->
                                []

                            [ _ ] ->
                                l

                            x :: y :: xs ->
                                if x == y then
                                    do (y :: xs)

                                else
                                    x :: do (y :: xs)
                in
                do l
                    |> String.fromList
           )


dropEvery : String -> Int -> String
dropEvery s num =
    String.toList s
        |> (\l ->
                let
                    do ll n =
                        case ll of
                            [] ->
                                []

                            xs ->
                                take (n - 1) xs ++ do (drop n xs) n
                in
                do l num
                    |> String.fromList
           )



-- String.join


clap : String -> String
clap =
    String.words
        >> intersperse " :clap: "
        >> String.concat
