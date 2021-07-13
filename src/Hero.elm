module Hero exposing (Hero, draw, getEdges, init, moveHero, startMove)

import Svg exposing (Svg, g, path)
import Html.Attributes exposing (attribute, id)
import CoreTypes exposing (Dir(..))
import Svg.Attributes exposing (d, fill, fillRule, style, transform, viewBox, width)


type alias Hero =
    { x : Float
    , y : Float
    , moveRight : Bool
    , moveLeft : Bool
    , moveUp : Bool
    , moveDown : Bool
    , heroDir : Dir
    }

heroWidth : number
heroWidth =
    90

speed : number
speed =
    15

init : () -> Hero
init _ =
    Hero 500 100 False False False False None


draw : Hero -> Svg msg
draw hero =
    let
        w =
            String.fromFloat heroWidth

        offset =
            String.fromFloat (heroWidth / 2)

        hero =
            case hero.heroDir of
                Left ->
                    [ g [ transform <| "translate(" ++ offset ++ ") scale(-1 1)" ]
                        [ drawRunning w ]
                    ]

                Right ->
                    [ g [ transform <| "translate(-" ++ offset ++ ")" ]
                        [ drawRunning w ]
                    ]

                Up ->
                    [ g [ transform <| "translate(" ++ offset ++ ")" ]
                        [ drawRunning w ]
                    ]

                Down ->
                    [ g [ transform <| "translate(-" ++ offset ++ ") scale(1 -1)" ]
                        [ drawRunning w ]
                    ]

                None ->
                    [ g [ transform <| "translate(-" ++ offset ++ ")" ]
                        [ drawStraight w ]
                    ]
    in
    g [ transform <| "translate(" ++ String.fromFloat hero.x ++ ")" ]
        hero


moveHero : Hero -> Hero
moveHero hero =
    case hero.heroDir of
        Left ->
            { hero | x = hero.x - speed }

        Right ->
            { hero | x = hero.x + speed }
        
        Up ->
            { hero | y = hero.y + speed }

        Down ->
            { hero | y = hero.y - speed }

        None ->
            hero


startMove : Hero -> Hero
startMove hero =
    { hero | heroDir = direction hero }


getEdges : Hero -> ( Float, Float, Float, Float )
getEdges hero =
    ( hero.x - heroWidth / 2, hero.x + heroWidth / 2,  hero.y - heroWidth, hero.y + heroWidth)

direction : Hero -> Dir
direction { moveLeft, moveRight, moveDown, moveUp } =
    case ( moveLeft, moveRight, moveDown, moveUp ) of
        ( True, False, _, _ ) ->
            Left

        ( False, True, _, _ ) ->
            Right

        ( _, _, True, False ) ->
            Down

        ( _, _, False, True ) ->
            Up
        _ ->
            None