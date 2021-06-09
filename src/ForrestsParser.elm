module ForrestsParser exposing (process)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node


process input =
    Elm.Processing.process Elm.Processing.init input
        |> .declarations
        |> List.filterMap justTheFunctions
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


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
