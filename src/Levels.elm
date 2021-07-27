module Levels exposing (Level, drawClearedLevel, loadLevel)

import Dir exposing (Dir(..))
import Enemy exposing (Enemy, finalBoss, newBasicEnemy, newSunEnemy)
import Html exposing (Html)
import Messages exposing (Msg(..))
import Modals exposing (ModalType(..), drawModal)


type alias Level =
    Int


type alias LevelData =
    List Enemy


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
            [ finalBoss ( 50, 50 ) Right ]

        _ ->
            []


drawClearedLevel : Level -> List (Html Msg)
drawClearedLevel level =
    case level of
        3 ->
            [ drawModal WonMessage ]

        _ ->
            [ drawModal ClearedMessage ]
