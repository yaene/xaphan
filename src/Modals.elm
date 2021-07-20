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
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "background-color" "white"
            , style "border-radius" "10px"
            , style "min-height" "200px"
            , style "width" "200px"
            , style "padding" "10px 5px 10px 5px"
            ]
            [ div
                [ style "margin-bottom" "10px"
                ]
                [ text "Pause Menu" ]
            , div
                [ style "display" "flex"
                , style "flex-grow" "1"
                , style "flex-direction" "column"
                , style "justify-content" "center"
                ]
                [ modalButton "Resume" Resume
                , modalButton "Skip Level" NextLevel
                ]
            ]
        ]


modalButton : String -> Msg -> Html Msg
modalButton content msg =
    button [ onClick msg ] [ text content ]
