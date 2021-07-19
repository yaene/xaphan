module Levels exposing (Level, drawClearedLevel, loadLevel)

import Animation exposing (Animation)
import Dir exposing (Dir(..))
import Enemy exposing (Enemy, newBasicEnemy, newSunEnemy)
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
            [ newBasicEnemy ( 50, 50 ) Right ]

        2 ->
            [ newBasicEnemy ( 50, 50 ) Right
            , newBasicEnemy ( 400, 200 ) Left
            ]

        3 ->
            [ newSunEnemy ( 500, 200 ) Left ]

        _ ->
            []


drawClearedLevel : Level -> List (Html Msg)
drawClearedLevel level =
    case level of
        3 ->
            [ text "You won :-)" ]

        _ ->
            [ button [ onClick <| NextLevel (level + 1) ] [ text "Next Level" ] ]
