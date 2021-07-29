module Levels exposing (Level, drawClearedLevel, loadLevel)

import Dir exposing (Dir(..))
import Enemy exposing (Enemy, EnemyType(..), finalBoss, newBasicEnemy, newEnvironmentalEnemy, newSpiralEnemy, newSunEnemy)
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
            [ newSunEnemy ( 200, 100 ) Right
            , newSunEnemy ( 500, 200 ) Left
            , newSunEnemy ( 100, 300 ) Right
            ]

        3 ->
            [ newSpiralEnemy ( 800, 100 ) Left -700
            , newSpiralEnemy ( 100, 300 ) Right 0
            ]

        4 ->
            [ finalBoss ( 50, 200 ) Right
            , newEnvironmentalEnemy ( 100, 0 ) 3000
            , newEnvironmentalEnemy ( 900, 0 ) 3500
            ]

        _ ->
            []


{-| draw the level cleared message
-}
drawClearedLevel : Level -> List (Html Msg)
drawClearedLevel level =
    case level of
        4 ->
            [ drawModal WonMessage ]

        _ ->
            [ drawModal ClearedMessage ]
