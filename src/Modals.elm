module Modals exposing (ModalType(..), drawModal)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import StyledComponents


{-| specifies the type of modal.
used to decide how to render a given modal.
-}
type ModalType
    = PauseMenu
    | ClearedMessage
    | WonMessage
    | LostMessage
    | ControlsInfo


{-| render a modal of a given type
-}
drawModal : ModalType -> Html Msg
drawModal modalType =
    case modalType of
        PauseMenu ->
            drawModal_ "Pause Menu"
                ""
                [ ( "Skip Level", NextLevel )
                , ( "Restart Level", Retry )
                , ( "Main Menu", Reset )
                , ( "Resume", Resume )
                ]

        ClearedMessage ->
            drawModal_ "Level Cleared" "Congrats you cleared the level!" [ ( "Next Level", NextLevel ) ]

        WonMessage ->
            drawModal_ "You Won :-)" "Congrats you have cleared the whole game!" [ ( "Main Menu", Reset ) ]

        LostMessage ->
            drawModal_ "Game Over :-(" "Better luck next time!" [ ( "Try again", Retry ) ]

        ControlsInfo ->
            drawModal_ "Controls"
                ("Move with arrow keys (or WASD) and shoot with Z (or SPACE). "
                    ++ "Dodge the enemies bullets and kill them with yours to win!"
                )
                [ ( "Ok", Reset ) ]


drawModal_ : String -> String -> List ( String, Msg ) -> Html Msg
drawModal_ title content actions =
    let
        contentDisplay =
            if String.isEmpty content then
                "none"

            else
                "block"
    in
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
            , style "color" "white"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "justify-content" "space-evenly"
            , style "background-color" "#333333"
            , style "border-radius" "15px"
            , style "min-height" "200px"
            , style "width" "300px"
            , style "padding" "20px"
            ]
            [ div
                [ style "margin-bottom" "10px"
                , style "font-size" "20px"
                , style "font-weight" "bold"
                ]
                [ text title ]
            , div
                [ style "margin-bottom" "10px"
                , style "font-size" "15px"
                , style "display" contentDisplay
                ]
                [ text content ]
            , div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "justify-content" "center"
                ]
                (actions |> List.map modalButton)
            ]
        ]


modalButton : ( String, Msg ) -> Html Msg
modalButton ( content, msg ) =
    StyledComponents.button content msg [ style "margin-bottom" "10px" ]
