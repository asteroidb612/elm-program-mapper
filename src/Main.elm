module Main exposing (main)

import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Html
import Html.Attributes
import Json.Encode


displayFunctionInfo : List String -> Html.Html a
displayFunctionInfo info =
    List.map Html.text info
        |> List.map List.singleton
        |> List.map (Html.div [ Html.Attributes.style "margin" "4em" ])
        |> Html.div []


extractFunctionInfo : RawFile -> List String
extractFunctionInfo input =
    Elm.Processing.process Elm.Processing.init input
        |> .declarations
        |> List.filterMap justTheFunctions
        |> List.sortBy .argCount
        |> List.map .typeAnnotation
        |> List.map TypeAnnotation.encode
        |> List.map (Json.Encode.encode 4)


type alias FunctionInfo =
    { name : String
    , argCount : Int
    , typeAnnotation : TypeAnnotation
    }


justTheFunctions : Node Declaration -> Maybe FunctionInfo
justTheFunctions node =
    case node of
        Node.Node _ (Declaration.FunctionDeclaration function) ->
            let
                { name, arguments } =
                    Node.value function.declaration

                writtenTypeAnnotation =
                    Maybe.map Node.value function.signature
                        |> Maybe.map .typeAnnotation
                        |> Maybe.map Node.value

                functionInfo typeInfo =
                    { name = Node.value name
                    , argCount = List.length arguments
                    , typeAnnotation = typeInfo
                    }
            in
            Maybe.map functionInfo writtenTypeAnnotation

        _ ->
            Nothing


parseThenProcess : String -> Html.Html a
parseThenProcess input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed Parsing: "
                ++ Debug.toString e
                |> Html.text

        Ok parsedElmCode ->
            extractFunctionInfo parsedElmCode
                |> displayFunctionInfo


main : Html.Html a
main =
    parseThenProcess ellie


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
