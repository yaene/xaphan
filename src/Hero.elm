module Hero exposing (Hero, HeroBullet, animateHero, animateHeroBullets, drawHero, heroHeight, heroWidth, init, moveHero, startMove, startShooting)

import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsX, moveBy)
import Messages exposing (Msg(..))
import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


type alias Hero =
    { x : Int
    , y : Int
    , moveRight : Bool
    , moveLeft : Bool
    , moveUp : Bool
    , moveDown : Bool
    , isShootKeyPressed : Bool
    , shootBullet : Bool
    , shootBulletElapsed : Float
    , heroDir : Dir
    }


type alias HeroBullet =
    { pos : Pos
    , dx : Int
    , dy : Int
    }


type Dir
    = Up
    | UpRight
    | Right
    | DownRight
    | Down
    | DownLeft
    | Left
    | UpLeft
    | None


heroWidth : Int
heroWidth =
    90


heroHeight : Int
heroHeight =
    120


heroSpeed : number
heroSpeed =
    15


init : () -> Hero
init _ =
    Hero 500 800 False False False False False False 0 None


animateHero : Float -> Hero -> List HeroBullet -> ( Hero, List HeroBullet )
animateHero elapsed hero bullets =
    let
        animatedbullets =
            animateHeroBullets hero bullets
    in
    ( hero
        |> moveHero
        |> animateShootBullet elapsed
    , animatedbullets
    )


drawHero : Hero -> Svg msg
drawHero hero =
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
            { hero | x = hero.x - heroSpeed }

        Right ->
            { hero | x = hero.x + heroSpeed }

        Up ->
            { hero | y = hero.y - heroSpeed }

        Down ->
            { hero | y = hero.y + heroSpeed }

        UpLeft ->
            { hero | y = hero.y - round (heroSpeed / sqrt 2), x = hero.x - round (heroSpeed / sqrt 2) }

        UpRight ->
            { hero | y = hero.y - round (heroSpeed / sqrt 2), x = hero.x + round (heroSpeed / sqrt 2) }

        DownLeft ->
            { hero | y = hero.y + round (heroSpeed / sqrt 2), x = hero.x - round (heroSpeed / sqrt 2) }

        DownRight ->
            { hero | y = hero.y + round (heroSpeed / sqrt 2), x = hero.x + round (heroSpeed / sqrt 2) }

        None ->
            hero


startMove : Hero -> Hero
startMove hero =
    { hero | heroDir = direction hero }


startShooting hero =
    { hero | isShootKeyPressed = True }


shootBullet : Hero -> HeroBullet
shootBullet hero =
    HeroBullet (moveBy ( heroWidth // 2, heroHeight ) ( hero.x, hero.y )) 0 5


animateShootBullet : Float -> Hero -> Hero
animateShootBullet elapsed hero =
    let
        shootBulletElapsed =
            hero.shootBulletElapsed + elapsed

        interval =
            1000
    in
    if hero.isShootKeyPressed == True then
        if shootBulletElapsed > interval then
            { hero | shootBulletElapsed = shootBulletElapsed - interval, shootBullet = True }

        else
            { hero | shootBulletElapsed = shootBulletElapsed, shootBullet = False }

    else
        hero


animateHeroBullets : Hero -> List HeroBullet -> List HeroBullet
animateHeroBullets hero bullets =
    bullets
        |> List.map animateHeroBullet
        |> (++) [ shootBullet hero ]


animateHeroBullet bullet =
    let
        ( x, y ) =
            bullet.pos
    in
    { bullet | pos = ( x, y + bullet.dy ) }


direction : Hero -> Dir
direction { moveLeft, moveRight, moveUp, moveDown } =
    case ( ( moveLeft, moveRight ), ( moveDown, moveUp ) ) of
        ( ( True, False ), ( False, False ) ) ->
            Left

        ( ( False, True ), ( False, False ) ) ->
            Right

        ( ( False, True ), ( True, False ) ) ->
            DownRight

        ( ( False, True ), ( False, True ) ) ->
            UpRight

        ( ( True, False ), ( False, True ) ) ->
            UpLeft

        ( ( True, False ), ( True, False ) ) ->
            DownLeft

        ( ( False, False ), ( True, False ) ) ->
            Down

        ( ( False, False ), ( False, True ) ) ->
            Up

        _ ->
            None
