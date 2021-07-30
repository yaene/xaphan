module StyledComponents exposing (button)

import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Messages exposing (Msg)


secondaryColor : String
secondaryColor =
    "gray"


button : String -> Msg -> List (Attribute Msg) -> Html Msg
button content msg attributes =
    Html.button
        ([ onClick msg
         , style "text-align" "center"
         , style "display" "inline-block"
         , style "color" "white"
         , style "font-size" "15px"
         , style "border" "none"
         , style "cursor" "pointer"
         , style "padding" "8px 15px"
         , style "background-color" secondaryColor
         ]
            ++ attributes
        )
        [ text content ]
