module Modals exposing (ModalType(..), drawModal)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))


type ModalType
    = PauseMenu
    | ClearedMessage
    | WonMessage
    | LostMessage
    | ControlsInfo
    | SelectionPage


drawModal : ModalType -> Html Msg
drawModal modalType =
    case modalType of
        PauseMenu ->
            drawModal_ "Pause Menu"
                ""
                [ ( "Resume", Resume )
                , ( "Skip Level", NextLevel )
                , ( "Restart Level", Retry )
                ]

        ClearedMessage ->
            drawModal_ "Level Cleared" "Congrats you cleared the level!" [ ( "Next Level", NextLevel ) ]

        WonMessage ->
            drawModal_ "You Won :-)" "Congrats you have cleared the whole game!" []

        LostMessage ->
            drawModal_ "Game Over :-(" "Better luck next time!" [ ( "Try again", Retry ) ]

        ControlsInfo ->
            drawModal_ "Controls"
                ("Move with arrow keys (or WASD) and shoot with Z (or SPACE). "
                    ++ "Dodge the enemies bullets and kill them with yours to win!"
                )
                [ ( "Ok", Reset ) ]
        SelectionPage ->
            drawModal_ "Select the superpower you want to use" "Superpower 1: clear all enemy bullets\nSuperpower 2: Hero ATK double for 5 seconds" [("Superpower 1", SelectSuperpower1),("Superpower 2", SelectSuperpower2)]
        


drawModal_ : String -> String -> List ( String, Msg ) -> Html Msg
drawModal_ title content actions =
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
            , style "min-height" "100px"
            , style "width" "200px"
            , style "padding" "10px 20px 10px 20px"
            ]
            [ div
                [ style "margin-bottom" "10px"
                , style "font-size" "20px"
                , style "font-weight" "bold"
                ]
                [ text title ]
            , div
                [ style "margin-bottom" "10px"
                , style "font-size" "12px"
                ]
                [ text content ]
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
