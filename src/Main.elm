module Main exposing (main)

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
import Url exposing (percentEncode)


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { code = Sample.ellie, link = "" }
                , Http.get
                    { url = codeUrl
                    , expect = Http.expectString GotCode
                    }
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


arrangeRendered model renderdFunc =
    Html.div []
        [ Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "bottom" "0"
            , Attr.style "left" "0"
            ]
            [ Html.textarea [ Events.onInput UpdateInput ] [] ]
        , Html.hr [] []
        , displayFunctionInfo renderdFunc
        , Html.pre
            [ Attr.style "text-align" "left"
            , Attr.style "margin" "4em"
            ]
            [ Html.text model ]
        ]


view { code, link } =
    Html.div []
        [ Html.iframe
            [ Attr.src link

            --, Attr.style "position" "fixed"
            --, Attr.style "top" "0"
            --, Attr.style "width" "100vw"
            , Attr.style "width" "100%"
            , Attr.style "height" "30em"
            ]
            []
        , Parsing.parseThenProcess code
            |> List.map renderFunc
            |> arrangeRendered code
        ]



{- Update -}


update msg model =
    case msg of
        UpdateInput change ->
            ( model
            , Http.get
                { url = change
                , expect = Http.expectString GotCode
                }
            )

        GotCode (Ok code) ->
            ( { code = code
              , link =
                    code
                        |> Parsing.parseThenProcess
                        |> Graphing.encodeGraphViz
                        |> newGraphLink
              }
            , Cmd.none
            )

        GotCode (Err e) ->
            ( model, Cmd.none )


type alias Model =
    { code : String, link : String }


type Msg
    = UpdateInput String
    | GotCode (Result Http.Error String)


codeUrl =
    "https://raw.githubusercontent.com/erkal/kite/master/src/Main.elm"


newGraphLink : String -> String
newGraphLink code =
    "https://edotor.net?engine=dot#" ++ percentEncode code
