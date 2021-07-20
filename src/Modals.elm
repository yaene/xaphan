module Modals exposing (ModalType(..), drawModal)

import Html exposing (Html, button, div, text)
import Html.Attributes as HtmlAttr exposing (style)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))


type ModalType
    = PauseMenu


drawModal : ModalType -> Html Msg
drawModal modalType =
    case modalType of
        PauseMenu ->
            drawModal_


drawModal_ : Html Msg
drawModal_ =
    div
        [ style "position" "fixed"
        , style "left" "0"
        , style "top" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background-color" "rgba(0,0,0,0.5)"
        ]
        [ div [] [ text "Pause Menu" ]
        , div [] [ button [ onClick Resume ] [ text "Resume" ] ]
        ]
