module Modals exposing (ModalType, drawModal)

import Html exposing (Html, text)
import Messages exposing (Msg)


type ModalType
    = PauseMenu


drawModal : ModalType -> Html Msg
drawModal modalType =
    case modalType of
        PauseMenu ->
            drawModal_


drawModal_ : Html Msg
drawModal_ =
    text "Paused"
