port module Main exposing (main)

import Browser
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Html
import Html.Attributes as Attr
import Json.Encode


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



--|> List.map .typeAnnotation
--|> List.map TypeAnnotation.encode
--|> List.map (Json.Encode.encode 4)


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
        Expression.FunctionOrValue _ name ->
            [ name ]

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
            {- TODO Add expressions in let expression -}
            []

        Expression.CaseExpression block ->
            {- TODO Add expressions in case expression -}
            []

        Expression.LambdaExpression lambda ->
            {- TODO Add expressions in lambda -}
            []

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
        , Html.pre
            [ Attr.style "text-align" "left"
            , Attr.style "margin" "4em"
            ]
            [ Html.text ellie ]
        ]


encodeGraphViz funcs =
    "digraph {a -> b}"


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( ()
                , newGraph
                    (parseThenProcess ellie
                        |> encodeGraphViz
                    )
                )
        , view =
            always
                (parseThenProcess ellie
                    |> List.map renderFunc
                    |> arrangeRendered
                )
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type Msg
    = Msg


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
