module Modals exposing (ModalType(..), drawModal)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))


type ModalType
    = PauseMenu


drawModal : ModalType -> Html Msg
drawModal modalType =
    case modalType of
        PauseMenu ->
            drawModal_ "Pause Menu" [ ( "Resume", Resume ), ( "Skip Level", NextLevel ) ]


drawModal_ : String -> List ( String, Msg ) -> Html Msg
drawModal_ title actions =
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
                [ text title ]
            , div
                [ style "display" "flex"
                , style "flex-grow" "1"
                , style "flex-direction" "column"
                , style "justify-content" "center"
                ]
                (actions |> List.map modalButton)
            ]
        ]


modalButton : ( String, Msg ) -> Html Msg
modalButton ( content, msg ) =
    button
        [ onClick msg
        , style "margin-bottom" "10px"
        ]
        [ text content ]
