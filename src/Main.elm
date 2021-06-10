module Main exposing (main)

import DrewsShortcuts
import Elm.Parser
import ElmCodeSamples exposing (ellie, forrestsSumOfMultiples)
import ForrestsMapper
import Html


parseThenProcess : String -> Html.Html a
parseThenProcess input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed Parsing: "
                ++ Debug.toString e
                |> Html.text

        Ok v ->
            ForrestsMapper.process v
                |> DrewsShortcuts.displayList


main : Html.Html a
main =
    parseThenProcess ellie
