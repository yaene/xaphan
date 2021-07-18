module Levels exposing (Level(..), loadLevel)

import Animation exposing (Animation)
import Dir exposing (Dir(..))
import Enemy exposing (Enemy)


type Level
    = Level1


type alias LevelData =
    List Enemy


loadLevel : Level -> LevelData
loadLevel level =
    case level of
        Level1 ->
            [ Enemy ( 50, 50 ) 2 0 Right (Animation 0 1500 False True) (Animation 0 1000 False True) ]
