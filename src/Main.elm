port module Main exposing (main)

import Browser
import Graphing
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Encode
import Parsing
import Sample
import Set


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( Sample.ellie
                , Cmd.batch
                    [ Sample.ellie
                        |> Parsing.parseThenProcess
                        |> Graphing.encodeGraphViz
                        |> newGraph
                    , Http.get
                        { url = codeUrl
                        , expect = Http.expectString GotCode
                        }
                    ]
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



{- View -}


displayFunctionInfo : List String -> Html.Html a
displayFunctionInfo info =
    List.map Html.text info
        |> List.map List.singleton
        |> List.map (Html.div [ Attr.style "margin" "4em" ])
        |> Html.div []


renderFunc { name, dependencies } =
    name ++ "::[" ++ String.join ", " dependencies ++ "]"


arrangeRendered renderdFunc =
    Html.div []
        [ Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            ]
            [ Html.textarea [ Events.onInput UpdateInput ] [] ]
        , Html.hr [] []
        , displayFunctionInfo renderdFunc
        , Html.pre
            [ Attr.style "text-align" "left"
            , Attr.style "margin" "4em"
            ]
            [ Html.text Sample.ellie ]
        ]


view model =
    Parsing.parseThenProcess model
        |> List.map renderFunc
        |> arrangeRendered



{- Update -}


update msg model =
    case msg of
        UpdateInput change ->
            ( change
                |> Debug.log "changedModel"
            , change
                |> Parsing.parseThenProcess
                |> Graphing.encodeGraphViz
                |> newGraph
            )

        GotCode (Ok code) ->
            ( code
                |> Debug.log "changedModel"
            , code
                |> Parsing.parseThenProcess
                |> Graphing.encodeGraphViz
                |> newGraph
            )

        GotCode (Err e) ->
            let
                _ =
                    Debug.log "Error getting code" e
            in
            ( model, Cmd.none )


type alias Model =
    String


type Msg
    = UpdateInput String
    | GotCode (Result Http.Error String)


codeUrl =
    "https://raw.githubusercontent.com/SocietyLibrary/debate_map_guide/import-review/src/Page/Papers/Paper_/Paragraphs.elm"


port newGraph : String -> Cmd msg
