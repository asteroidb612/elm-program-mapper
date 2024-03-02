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
            , Attr.style "top" "0"
            , Attr.style "right" "0"
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
        [ Html.iframe [ Attr.src link ] []
        , Html.hr [] []
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
newGraphLink _ =
    "https://edotor.net/?engine=dot#digraph%20G%20%7B%0A%20%20rankdir%3DLR%0A%20%20graph%20%5Bsplines%3Dortho%2C%20ordering%3Dout%2C%20overlap%20%3D%20scale%3B%5D%0A%20%20node%20%5Bshape%3Drect%2C%20style%3D%22rounded%22%5D%0A%20%20edge%20%5Barrowhead%3Dnone%2C%20arrowtail%3Dnone%5D%0A%0A%20%200%20-%3E%201%0A%20%200%20-%3E%202%0A%20%200%20-%3E%208%0A%20%204%20-%3E%2029%0A%20%204%20-%3E%2067%0A%20%206%20-%3E%205%0A%20%206%20-%3E%2010%0A%20%206%20-%3E%2013%0A%20%206%20-%3E%2017%0A%20%208%20-%3E%207%0A%20%208%20-%3E%209%0A%20%209%20-%3E%2025%0A%20%2016%20-%3E%2013%0A%20%2017%20-%3E%209%0A%20%2017%20-%3E%2010%0A%20%2017%20-%3E%2011%0A%20%2017%20-%3E%2012%0A%20%2017%20-%3E%2013%0A%20%2017%20-%3E%2014%0A%20%2017%20-%3E%2015%0A%20%2017%20-%3E%2016%0A%20%2017%20-%3E%2017%0A%20%2018%20-%3E%203%0A%20%2018%20-%3E%2019%0A%20%2018%20-%3E%2020%0A%20%2018%20-%3E%2022%0A%20%2018%20-%3E%2023%0A%20%2019%20-%3E%2021%0A%20%2019%20-%3E%2023%0A%20%2023%20-%3E%2024%0A%20%2028%20-%3E%2027%0A%20%2029%20-%3E%2030%0A%20%2030%20-%3E%2025%0A%20%2030%20-%3E%2031%0A%20%2030%20-%3E%2032%0A%20%2030%20-%3E%2033%0A%20%2030%20-%3E%2035%0A%20%2030%20-%3E%2052%0A%20%2030%20-%3E%2053%0A%20%2032%20-%3E%2025%0A%20%2033%20-%3E%2025%0A%20%2033%20-%3E%2038%0A%20%2033%20-%3E%2039%0A%20%2033%20-%3E%2040%0A%20%2033%20-%3E%2041%0A%20%2033%20-%3E%2043%0A%20%2033%20-%3E%2045%0A%20%2033%20-%3E%2046%0A%20%2033%20-%3E%2047%0A%20%2038%20-%3E%2034%0A%20%2038%20-%3E%2035%0A%20%2038%20-%3E%2055%0A%20%2039%20-%3E%2013%0A%20%2039%20-%3E%2026%0A%20%2039%20-%3E%2034%0A%20%2039%20-%3E%2035%0A%20%2039%20-%3E%2036%0A%20%2039%20-%3E%2037%0A%20%2040%20-%3E%2034%0A%20%2041%20-%3E%2034%0A%20%2043%20-%3E%2034%0A%20%2043%20-%3E%2042%0A%20%2043%20-%3E%2055%0A%20%2045%20-%3E%2034%0A%20%2045%20-%3E%2044%0A%20%2046%20-%3E%2034%0A%20%2047%20-%3E%2034%0A%20%2052%20-%3E%2025%0A%20%2052%20-%3E%2048%0A%20%2052%20-%3E%2049%0A%20%2052%20-%3E%2050%0A%20%2052%20-%3E%2051%0A%20%2053%20-%3E%2025%0A%20%2053%20-%3E%2060%0A%20%2053%20-%3E%2061%0A%20%2053%20-%3E%2062%0A%20%2053%20-%3E%2063%0A%20%2053%20-%3E%2064%0A%20%2055%20-%3E%2054%0A%20%2056%20-%3E%2054%0A%20%2057%20-%3E%2054%0A%20%2058%20-%3E%2054%0A%20%2059%20-%3E%2054%0A%20%2060%20-%3E%2034%0A%20%2061%20-%3E%2034%0A%20%2062%20-%3E%2013%0A%20%2062%20-%3E%2028%0A%20%2062%20-%3E%2034%0A%20%2062%20-%3E%2035%0A%20%2062%20-%3E%2036%0A%20%2062%20-%3E%2037%0A%20%2062%20-%3E%2055%0A%20%2062%20-%3E%2057%0A%20%2062%20-%3E%2059%0A%20%2063%20-%3E%2013%0A%20%2063%20-%3E%2034%0A%20%2063%20-%3E%2055%0A%20%2063%20-%3E%2056%0A%20%2063%20-%3E%2057%0A%20%2063%20-%3E%2058%0A%20%2063%20-%3E%2059%0A%20%2064%20-%3E%2013%0A%20%2064%20-%3E%2034%0A%20%2064%20-%3E%2055%0A%20%2064%20-%3E%2056%0A%20%2064%20-%3E%2057%0A%20%2064%20-%3E%2059%0A%20%2067%20-%3E%2013%0A%20%2067%20-%3E%2065%0A%20%2067%20-%3E%2066%0A%20%2067%20-%3E%2068%0A%20%2067%20-%3E%2069%0A%20%2067%20-%3E%2070%0A%20%2067%20-%3E%2071%0A%20%2067%20-%3E%2072%0A%20%2067%20-%3E%2073%0A%20%2067%20-%3E%2074%0A%20%2067%20-%3E%2075%0A%20%2067%20-%3E%2077%0A%20%2067%20-%3E%2078%0A%20%2067%20-%3E%2079%0A%20%2067%20-%3E%2080%0A%20%2068%20-%3E%2066%0A%20%2070%20-%3E%2066%0A%20%2071%20-%3E%2066%0A%20%2072%20-%3E%2066%0A%20%2075%20-%3E%2066%0A%20%2077%20-%3E%2026%0A%20%2077%20-%3E%2066%0A%20%2077%20-%3E%2076%0A%20%2078%20-%3E%2066%0A%20%2080%20-%3E%2066%0A%0A%20%200%20%5Blabel%3D%22init%22%5D%0A%20%201%20%5Blabel%3D%22graphFilesDecoder%22%5D%0A%20%202%20%5Blabel%3D%22getWindowSize%22%5D%0A%20%203%20%5Blabel%3D%22mousePosition%22%5D%0A%20%204%20%5Blabel%3D%22view%22%5D%0A%20%205%20%5Blabel%3D%22encodeGraphFiles%22%5D%0A%20%206%20%5Blabel%3D%22update%22%5D%0A%20%207%20%5Blabel%3D%22defaultGraphFiles%22%5D%0A%20%208%20%5Blabel%3D%22initialModel%22%5D%0A%20%209%20%5Blabel%3D%22initialPan%22%5D%0A%20%2010%20%5Blabel%3D%22reheatForce%22%5D%0A%20%2011%20%5Blabel%3D%22setAlphaTarget%22%5D%0A%20%2012%20%5Blabel%3D%22stopAnimation%22%5D%0A%20%2013%20%5Blabel%3D%22present%22%5D%0A%20%2014%20%5Blabel%3D%22new%22%5D%0A%20%2015%20%5Blabel%3D%22setPresentWithoutrecording%22%5D%0A%20%2016%20%5Blabel%3D%22withNewGravityCenter%22%5D%0A%20%2017%20%5Blabel%3D%22updateHelper%22%5D%0A%20%2018%20%5Blabel%3D%22subscriptions%22%5D%0A%20%2019%20%5Blabel%3D%22keyDown%22%5D%0A%20%2020%20%5Blabel%3D%22animationFrame%22%5D%0A%20%2021%20%5Blabel%3D%22toKeyDownMsg%22%5D%0A%20%2022%20%5Blabel%3D%22toKeyUpMsg%22%5D%0A%20%2023%20%5Blabel%3D%22keyDecoder%22%5D%0A%20%2024%20%5Blabel%3D%22toKey%22%5D%0A%20%2025%20%5Blabel%3D%22layoutParams%22%5D%0A%20%2026%20%5Blabel%3D%22edgeIdToString%22%5D%0A%20%2027%20%5Blabel%3D%22vertexIdToString%22%5D%0A%20%2028%20%5Blabel%3D%22vertexIdsToString%22%5D%0A%20%2029%20%5Blabel%3D%22viewHelper%22%5D%0A%20%2030%20%5Blabel%3D%22guiColumns%22%5D%0A%20%2031%20%5Blabel%3D%22fpsView%22%5D%0A%20%2032%20%5Blabel%3D%22leftStripe%22%5D%0A%20%2033%20%5Blabel%3D%22leftBar%22%5D%0A%20%2034%20%5Blabel%3D%22menu%22%5D%0A%20%2035%20%5Blabel%3D%22menuHeaderButton%22%5D%0A%20%2036%20%5Blabel%3D%22columnHeader%22%5D%0A%20%2037%20%5Blabel%3D%22commonCellProperties%22%5D%0A%20%2038%20%5Blabel%3D%22leftBarContentForFiles%22%5D%0A%20%2039%20%5Blabel%3D%22leftBarContentForListsOfBagsVerticesAndEdges%22%5D%0A%20%2040%20%5Blabel%3D%22leftBarContentForGraphOperations%22%5D%0A%20%2041%20%5Blabel%3D%22leftBarContentForGraphQueries%22%5D%0A%20%2042%20%5Blabel%3D%22buttonWithIconAndText%22%5D%0A%20%2043%20%5Blabel%3D%22leftBarContentForGraphGenerators%22%5D%0A%20%2044%20%5Blabel%3D%22runButton%22%5D%0A%20%2045%20%5Blabel%3D%22leftBarContentForAlgorithmVisualizations%22%5D%0A%20%2046%20%5Blabel%3D%22leftBarContentForGamesOnGraphs%22%5D%0A%20%2047%20%5Blabel%3D%22leftBarContentForPreferences%22%5D%0A%20%2048%20%5Blabel%3D%22oneClickButtonGroup%22%5D%0A%20%2049%20%5Blabel%3D%22oneClickButton%22%5D%0A%20%2050%20%5Blabel%3D%22radioButtonGroup%22%5D%0A%20%2051%20%5Blabel%3D%22radioButton%22%5D%0A%20%2052%20%5Blabel%3D%22toolButtons%22%5D%0A%20%2053%20%5Blabel%3D%22rightBar%22%5D%0A%20%2054%20%5Blabel%3D%22labelAttr%22%5D%0A%20%2055%20%5Blabel%3D%22textInput%22%5D%0A%20%2056%20%5Blabel%3D%22sliderInput%22%5D%0A%20%2057%20%5Blabel%3D%22checkbox%22%5D%0A%20%2058%20%5Blabel%3D%22labelPositionInput%22%5D%0A%20%2059%20%5Blabel%3D%22colorPicker%22%5D%0A%20%2060%20%5Blabel%3D%22history%22%5D%0A%20%2061%20%5Blabel%3D%22selector%22%5D%0A%20%2062%20%5Blabel%3D%22bags%22%5D%0A%20%2063%20%5Blabel%3D%22vertexPreferences%22%5D%0A%20%2064%20%5Blabel%3D%22edgePreferences%22%5D%0A%20%2065%20%5Blabel%3D%22wheelDeltaY%22%5D%0A%20%2066%20%5Blabel%3D%22emptySvgElement%22%5D%0A%20%2067%20%5Blabel%3D%22mainSvg%22%5D%0A%20%2068%20%5Blabel%3D%22maybeGravityLines%22%5D%0A%20%2069%20%5Blabel%3D%22viewMapScale%22%5D%0A%20%2070%20%5Blabel%3D%22maybeBrushedEdge%22%5D%0A%20%2071%20%5Blabel%3D%22maybeHighlightsOnSelectedEdges%22%5D%0A%20%2072%20%5Blabel%3D%22maybeHighlightOnMouseOveredEdges%22%5D%0A%20%2073%20%5Blabel%3D%22maybeHighlightsOnSelectedVertices%22%5D%0A%20%2074%20%5Blabel%3D%22maybeHighlightOnMouseOveredVertices%22%5D%0A%20%2075%20%5Blabel%3D%22maybeRectAroundSelectedVertices%22%5D%0A%20%2076%20%5Blabel%3D%22arrow%22%5D%0A%20%2077%20%5Blabel%3D%22viewEdges%22%5D%0A%20%2078%20%5Blabel%3D%22viewVertices%22%5D%0A%20%2079%20%5Blabel%3D%22viewHulls%22%5D%0A%20%2080%20%5Blabel%3D%22maybeViewGravityCenters%22%5D%0A%7D"
