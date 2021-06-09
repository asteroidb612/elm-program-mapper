module Main exposing (main)

import DrewsShortcuts
import Elm.Parser
import ElmCodeSamples exposing (ellie, forrestsSumOfMultiples)
import ForrestsParser
import Html


process : String -> Html.Html a
process input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed Parsing: "
                ++ Debug.toString e
                |> Html.text

        Ok v ->
            ForrestsParser.process v
                |> DrewsShortcuts.displayList


main =
    process ellie
