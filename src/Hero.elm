module Hero exposing (Hero, draw, init, moveHero, startMove)

import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


type alias Hero =
    { x : Int
    , y : Int
    , moveRight : Bool
    , moveLeft : Bool
    , moveUp : Bool
    , moveDown : Bool
    , heroDir : Dir
    }


type Dir
    = Up
    | Right
    | Down
    | Left
    | None


heroWidth : Int
heroWidth =
    90


heroHeight : Int
heroHeight =
    120


speed : number
speed =
    15


init : () -> Hero
init _ =
    Hero 500 800 False False False False None


draw : Hero -> Svg msg
draw hero =
    Svg.rect
        [ SvgAttr.x <| String.fromInt hero.x
        , SvgAttr.y <| String.fromInt hero.y
        , SvgAttr.height <| String.fromInt heroHeight
        , SvgAttr.width <| String.fromInt heroWidth
        , SvgAttr.fill "blue"
        ]
        []


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


direction : Hero -> Dir
direction { moveLeft, moveRight } =
    case ( moveLeft, moveRight ) of
        ( True, False ) ->
            Left

        ( False, True ) ->
            Right

        _ ->
            None
