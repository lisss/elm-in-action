module MorseCode exposing (decode)

import Dict exposing (Dict)
import MorseCodes


codes =
    MorseCodes.get


decode : String -> String
decode code =
    code
        |> String.trim
        |> String.split "  "
        |> List.map (String.words >> List.filterMap (\x -> Dict.get x codes))
        |> List.intersperse [ " " ]
        |> List.concat
        |> String.concat
