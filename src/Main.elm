module Main exposing (main)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node
import ElmCodeSamples exposing (ellie, smallExampleFromElmSyntaxDocumentation)
import Html
import Html.Attributes


process : String -> Html.Html a
process input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed Parsing: "
                ++ Debug.toString e
                |> Html.text

        Ok v ->
            Elm.Processing.process Elm.Processing.init v
                |> .declarations
                |> List.filterMap justTheFunctions
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
                |> displayList


displayList : List String -> Html.Html a
displayList l =
    l
        |> List.map Html.text
        |> List.map List.singleton
        |> List.map (Html.div [ Html.Attributes.style "margin" "4em" ])
        |> Html.div []


justTheFunctions node =
    case node of
        Node.Node _ (Declaration.FunctionDeclaration { declaration }) ->
            case declaration of
                Node.Node _ { name, arguments } ->
                    case name of
                        Node.Node _ n ->
                            Just ( n, List.length arguments )

        _ ->
            Nothing


main =
    process ellie
