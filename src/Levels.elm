module Levels exposing (Level, drawClearedLevel, loadLevel)

import Animation exposing (Animation)
import Dir exposing (Dir(..))
import Enemy exposing (Enemy)
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))


type alias Level =
    Int


type alias LevelData =
    List Enemy


loadLevel : Level -> LevelData
loadLevel level =
    case level of
        1 ->
            [ Enemy ( 50, 50 ) 2 0 Right (Animation 0 1500 False True) (Animation 0 1000 False True) ]

        2 ->
            [ Enemy ( 50, 50 ) 5 0 Right (Animation 0 1500 False True) (Animation 0 800 False True)
            , Enemy ( 400, 200 ) 5 0 Left (Animation 0 1500 False True) (Animation 0 1000 False True)
            ]

        _ ->
            []


drawClearedLevel : Level -> List (Html Msg)
drawClearedLevel level =
    case level of
        2 ->
            [ text "You won :-)" ]

        _ ->
            [ button [ onClick <| NextLevel (level + 1) ] [ text "Next Level" ] ]
