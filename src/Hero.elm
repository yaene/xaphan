module Hero exposing (Hero, HeroBullet, animateHero, animateHeroBullets, bulletHeight, bulletWidth, drawHero, drawHeroBullets, heroHeight, heroWidth, init, moveHero, shootBullet, startMove)

import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsX, moveBy)
import Messages exposing (Msg(..))
import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


type alias Hero =
    { pos : Pos
    , hp : Int
    , moveRight : Bool
    , moveLeft : Bool
    , moveUp : Bool
    , moveDown : Bool
    , heroDir : Dir
    }


type alias HeroBullet =
    { posBullet : Pos
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


bulletWidth =
    10


bulletHeight =
    20


heroSpeed : number
heroSpeed =
    15


init : () -> Hero
init _ =
    Hero ( 500, 800 ) 3 False False False False None


animateHero : Float -> Hero -> List HeroBullet -> ( Hero, List HeroBullet )
animateHero elapsed hero bullets =
    let
        animatedbullets =
            animateHeroBullets hero bullets
    in
    ( hero
        |> moveHero
    , animatedbullets
    )


drawHero : Hero -> Svg msg
drawHero hero =
    Svg.g []
        [ Svg.rect
            [ SvgAttr.x <| String.fromInt <| Tuple.first hero.pos
            , SvgAttr.y <| String.fromInt <| Tuple.second hero.pos
            , SvgAttr.height <| String.fromInt heroHeight
            , SvgAttr.width <| String.fromInt heroWidth
            , SvgAttr.fill "blue"
            ]
            []
        , drawLives hero.hp
        ]


drawLives : Int -> Svg msg
drawLives hp =
    Svg.g
        [ SvgAttr.transform "translate(880, 960)"
        ]
        (List.range 0 (hp - 1)
            |> List.map drawHeart
        )


drawHeart : Int -> Svg msg
drawHeart n =
    Svg.g
        [ SvgAttr.transform <| "translate(" ++ (String.fromInt <| n * 40) ++ ")"
        ]
        [ Svg.svg
            [ SvgAttr.viewBox "0 0 343.422 343.422"
            , SvgAttr.width "40"
            , SvgAttr.height "40"
            , SvgAttr.fill "blue"
            ]
            [ Svg.path [ SvgAttr.d "M254.791,33.251c-46.555,0-76.089,51.899-83.079,51.899c-6.111,0-34.438-51.899-83.082-51.899c-47.314,0-85.947,39.021-88.476,86.27c-1.426,26.691,7.177,47.001,19.304,65.402c24.222,36.76,130.137,125.248,152.409,125.248c22.753,0,127.713-88.17,152.095-125.247c12.154-18.483,20.731-38.711,19.304-65.402C340.738,72.272,302.107,33.251,254.791,33.251" ] []
            ]
        ]


moveHero : Hero -> Hero
moveHero hero =
    let
        ( x, y ) =
            hero.pos
    in
    case hero.heroDir of
        Left ->
            { hero | pos = ( x - heroSpeed, y ) }

        Right ->
            { hero | pos = ( x + heroSpeed, y ) }

        Up ->
            { hero | pos = ( x, y - heroSpeed ) }

        Down ->
            { hero | pos = ( x, y + heroSpeed ) }

        UpLeft ->
            { hero | pos = ( x - round (heroSpeed / sqrt 2), y - round (heroSpeed / sqrt 2) ) }

        UpRight ->
            { hero | pos = ( x + round (heroSpeed / sqrt 2), y - round (heroSpeed / sqrt 2) ) }

        DownLeft ->
            { hero | pos = ( x - round (heroSpeed / sqrt 2), y + round (heroSpeed / sqrt 2) ) }

        DownRight ->
            { hero | pos = ( x + round (heroSpeed / sqrt 2), y + round (heroSpeed / sqrt 2) ) }

        None ->
            hero


startMove : Hero -> Hero
startMove hero =
    { hero | heroDir = direction hero }


shootBullet : Hero -> HeroBullet
shootBullet hero =
    HeroBullet (moveBy ( heroWidth // 2, -bulletHeight ) hero.pos) 0 -5


animateHeroBullets : Hero -> List HeroBullet -> List HeroBullet
animateHeroBullets hero bullets =
    bullets
        |> List.map animateHeroBullet


animateHeroBullet bullet =
    let
        ( x, y ) =
            bullet.posBullet
    in
    { bullet | posBullet = ( x, y + bullet.dy ) }


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


drawHeroBullets : List HeroBullet -> List (Svg Msg)
drawHeroBullets enemyBullets =
    enemyBullets |> List.map drawHeroBullet


drawHeroBullet : HeroBullet -> Svg Msg
drawHeroBullet bullet =
    let
        ( x, y ) =
            bullet.posBullet
    in
    Svg.rect
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.fill "green"
        , SvgAttr.width <| String.fromInt bulletWidth
        , SvgAttr.height <| String.fromInt bulletHeight
        ]
        []
