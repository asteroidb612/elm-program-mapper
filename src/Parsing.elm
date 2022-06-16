module Parsing exposing (parseThenProcess)

import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


namesThisDependsOn expression =
    case Node.value expression of
        Expression.FunctionOrValue mods name ->
            if mods == [] then
                [ name ]

            else
                []

        Expression.Application expressions ->
            List.concatMap (\e -> namesThisDependsOn e) expressions

        Expression.OperatorApplication _ _ first second ->
            List.concatMap (\e -> namesThisDependsOn e) [ first, second ]

        Expression.IfBlock e1 e2 e3 ->
            List.concatMap (\e -> namesThisDependsOn e) [ e1, e2, e3 ]

        Expression.Negation e ->
            namesThisDependsOn e

        Expression.TupledExpression expressions ->
            List.concatMap (\e -> namesThisDependsOn e) expressions

        Expression.ParenthesizedExpression e ->
            namesThisDependsOn e

        Expression.LetExpression block ->
            namesThisDependsOn block.expression
                ++ List.concatMap
                    (\declaration ->
                        case Node.value declaration of
                            Expression.LetFunction func ->
                                Node.value func.declaration
                                    |> .expression
                                    |> namesThisDependsOn

                            Expression.LetDestructuring _ nodeExpression ->
                                namesThisDependsOn nodeExpression
                    )
                    block.declarations

        Expression.CaseExpression block ->
            namesThisDependsOn block.expression
                ++ List.concatMap (\c -> namesThisDependsOn (Tuple.second c)) block.cases

        Expression.LambdaExpression lambda ->
            namesThisDependsOn lambda.expression

        Expression.RecordExpr recordSets ->
            List.concatMap
                (\recordSet ->
                    namesThisDependsOn (Tuple.second (Node.value recordSet))
                )
                recordSets

        Expression.RecordUpdateExpression _ recordSets ->
            List.concatMap
                (\recordSet ->
                    namesThisDependsOn (Tuple.second (Node.value recordSet))
                )
                recordSets

        Expression.ListExpr expressions ->
            List.concatMap (\e -> namesThisDependsOn e) expressions

        Expression.RecordAccess e _ ->
            namesThisDependsOn e

        _ ->
            []


parseThenProcess : String -> List FunctionInfo
parseThenProcess input =
    case Elm.Parser.parse input of
        Err e ->
            []

        Ok parsedElmCode ->
            extractFunctionInfo parsedElmCode


extractFunctionInfo : RawFile -> ParsedFile
extractFunctionInfo input =
    Elm.Processing.process Elm.Processing.init input
        |> .declarations
        |> List.filterMap justTheFunctions


type alias ParsedFile =
    List FunctionInfo


type alias FunctionInfo =
    { name : String
    , argCount : Int
    , dependencies : List String
    }


justTheFunctions : Node Declaration -> Maybe FunctionInfo
justTheFunctions node =
    case node of
        Node.Node _ (Declaration.FunctionDeclaration function) ->
            let
                { name, arguments, expression } =
                    Node.value function.declaration

                functionInfo =
                    { name = Node.value name
                    , argCount = List.length arguments
                    , dependencies = namesThisDependsOn expression
                    }
            in
            Just functionInfo

        _ ->
            Nothing
