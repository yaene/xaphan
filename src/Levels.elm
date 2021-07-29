module Levels exposing (Level, drawClearedLevel, loadLevel)

import Dir exposing (Dir(..))
import Enemy exposing (Enemy, newBasicEnemy, newSpiralEnemy, newSunEnemy)
import Html exposing (Html)
import Messages exposing (Msg(..))
import Modals exposing (ModalType(..), drawModal)


{-| defines the current level
-}
type alias Level =
    Int


type alias LevelData =
    List Enemy


{-| create the enemies for a given level
-}
loadLevel : Level -> LevelData
loadLevel level =
    case level of
        1 ->
            [ newBasicEnemy ( 50, 50 ) Right
            , newBasicEnemy ( 400, 200 ) Left
            ]

        2 ->
            [ newSunEnemy ( 500, 200 ) Left ]

        3 ->
            [ newSpiralEnemy ( 50, 50 ) Right ]

        _ ->
            []


{-| draw the level cleared message
-}
drawClearedLevel : Level -> List (Html Msg)
drawClearedLevel level =
    case level of
        3 ->
            [ drawModal WonMessage ]

        _ ->
            [ drawModal ClearedMessage ]
