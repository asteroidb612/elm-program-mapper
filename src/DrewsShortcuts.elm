module DrewsShortcuts exposing (displayList)

import Html
import Html.Attributes


displayList : List String -> Html.Html a
displayList l =
    l
        |> List.map Html.text
        |> List.map List.singleton
        |> List.map (Html.div [ Html.Attributes.style "margin" "4em" ])
        |> Html.div []
