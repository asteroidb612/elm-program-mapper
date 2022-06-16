module Graphing exposing (encodeGraphViz)

import Graph
import Graph.DOT exposing (defaultStyles)
import List.Extra as List
import Set


encodeGraphViz funcs =
    let
        topLevelDefs =
            List.map .name funcs
                |> Set.fromList

        filteredFuncs =
            funcs
                |> List.filter
                    (\{ name } ->
                        not (List.member name [ "main", "page" ])
                    )

        edges =
            filteredFuncs
                |> List.indexedMap
                    (\i { dependencies, name } ->
                        Set.fromList dependencies
                            |> Set.intersect topLevelDefs
                            |> Set.toList
                            |> List.map
                                (\dependency ->
                                    { name = name
                                    , dependency = dependency
                                    , i = i
                                    }
                                )
                    )
                |> List.concatMap identity
    in
    Graph.fromNodeLabelsAndEdgePairs
        (Debug.log "names" (List.map .name filteredFuncs))
        (List.filterMap
            (\{ i, dependency } ->
                List.findIndex (\func -> func.name == dependency) filteredFuncs
                    |> Maybe.map (\j -> ( i, j ))
            )
            edges
        )
        |> Graph.DOT.outputWithStyles
            { defaultStyles
                | graph = "splines=ortho, ratio=0.3, ordering=out"
            }
            (Just << identity)
            (always Nothing)
