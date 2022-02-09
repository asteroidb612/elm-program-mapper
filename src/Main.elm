module Main exposing (main)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node
import Html
import Html.Attributes


displayList : List String -> Html.Html a
displayList l =
    l
        |> List.map Html.text
        |> List.map List.singleton
        |> List.map (Html.div [ Html.Attributes.style "margin" "4em" ])
        |> Html.div []


process input =
    Elm.Processing.process Elm.Processing.init input
        |> .declarations
        |> List.filterMap justTheFunctions
        |> Debug.log "functions"
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


justTheFunctions node =
    case node of
        Node.Node _ (Declaration.FunctionDeclaration { declaration, signature }) ->
            let
                _ =
                    signature
                        |> (\x ->
                                case x of
                                    Nothing ->
                                        Debug.todo ""

                                    Just (Node.Node _ { typeAnnotation }) ->
                                        typeAnnotation
                                            |> Debug.log "sigs"
                           )
            in
            case declaration of
                Node.Node _ { name, arguments } ->
                    case name of
                        Node.Node _ n ->
                            Just ( n, List.length arguments )

        _ ->
            Nothing


parseThenProcess : String -> Html.Html a
parseThenProcess input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed Parsing: "
                ++ Debug.toString e
                |> Html.text

        Ok v ->
            process v
                |> displayList


main : Html.Html a
main =
    parseThenProcess ellie


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
