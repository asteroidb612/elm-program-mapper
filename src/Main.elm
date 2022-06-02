port module Main exposing (main)

import Browser
import DotLang as GV exposing (..)
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Encode
import Set


port newGraph : String -> Cmd msg


displayFunctionInfo : List String -> Html.Html a
displayFunctionInfo info =
    List.map Html.text info
        |> List.map List.singleton
        |> List.map (Html.div [ Attr.style "margin" "4em" ])
        |> Html.div []


extractFunctionInfo : RawFile -> List FunctionInfo
extractFunctionInfo input =
    Elm.Processing.process Elm.Processing.init input
        |> .declarations
        |> List.filterMap justTheFunctions


renderFunc { name, dependencies } =
    name ++ "::[" ++ String.join ", " dependencies ++ "]"


type alias FunctionInfo =
    { name : String
    , argCount : Int
    , typeAnnotation : TypeAnnotation
    , dependencies : List String
    }


justTheFunctions : Node Declaration -> Maybe FunctionInfo
justTheFunctions node =
    case node of
        Node.Node _ (Declaration.FunctionDeclaration function) ->
            let
                { name, arguments, expression } =
                    Node.value function.declaration

                writtenTypeAnnotation =
                    Maybe.map Node.value function.signature
                        |> Maybe.map .typeAnnotation
                        |> Maybe.map Node.value

                functionInfo typeInfo =
                    { name = Node.value name
                    , argCount = List.length arguments
                    , typeAnnotation = typeInfo
                    , dependencies = namesThisDependsOn expression
                    }
            in
            writtenTypeAnnotation
                |> Maybe.map functionInfo

        _ ->
            Nothing


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
            Debug.todo "Failed Parsing: "

        --    --++ Debug.toString e
        --    |> List.singleton
        Ok parsedElmCode ->
            extractFunctionInfo parsedElmCode


arrangeRendered renderdFunc =
    Html.div []
        [ displayFunctionInfo renderdFunc
        , Html.hr [] []
        , Html.textarea [ Events.onInput UpdateInput ] []
        , Html.pre
            [ Attr.style "text-align" "left"
            , Attr.style "margin" "4em"
            ]
            [ Html.text ellie ]
        ]


encodeGraphViz funcs =
    let
        makeEdge name dep =
            EdgeStmtNode
                (NodeId (ID dep) Nothing)
                (EdgeNode (NodeId (ID name) Nothing))
                []
                []

        edges =
            funcs
                |> List.filter
                    (\{ name } ->
                        not (List.member name [ "main", "page" ])
                    )
                |> List.concatMap
                    (\{ dependencies, name } ->
                        List.map (makeEdge name)
                            (Set.toList (Set.fromList dependencies))
                    )
    in
    toString (Dot Digraph Nothing edges)


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( ellie
                , ellie
                    |> parseThenProcess
                    |> encodeGraphViz
                    |> newGraph
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view model =
    parseThenProcess model
        |> List.map renderFunc
        |> arrangeRendered


update msg model =
    case msg of
        UpdateInput change ->
            ( change
                |> Debug.log "changedModel"
            , change
                |> parseThenProcess
                |> encodeGraphViz
                |> newGraph
            )


type alias Model =
    String


type Msg
    = UpdateInput String


ellie : String
ellie =
    """
module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
"""
