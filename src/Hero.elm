module Hero exposing
    ( Hero
    , HeroBullet
    , animateHero
    , animateHeroBullets
    , atkDouble
    , bulletHeight
    , bulletWidth
    , drawHero
    , drawHeroBullets
    , heroHeight
    , heroWidth
    , init
    , selectSuperPower
    , setSuperpower
    , shootBullet
    , startMove
    , startShooting
    , stopShooting
    , useSuperpower
    )

import Animation exposing (Animation, newAnimation, updateAnimation)
import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsDimensions)
import Messages exposing (Msg(..))
import Modals exposing (ModalType)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


{-| contains all the data for animating the hero
-}
type alias Hero =
    { pos : Pos
    , hp : Int
    , moveRight : Bool
    , moveLeft : Bool
    , moveUp : Bool
    , moveDown : Bool
    , heroDir : Dir
    , spSelection : Int
    , spInstance : Int
    , spElapsed : Float
    , atkDoubled : Bool
    , shootingAnimation : Animation
    }


{-| contains all the data for animating a hero bullet
-}
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


{-| specifies the hero's hitbox width
-}
heroWidth : Int
heroWidth =
    45


{-| specifies the hero's hitbox height
-}
heroHeight : Int
heroHeight =
    60


{-| specifies the hero's bullets hitbox width
-}
bulletWidth : number
bulletWidth =
    10


{-| specifies the hero's bullets hitbox height
-}
bulletHeight : number
bulletHeight =
    20


heroSpeed : number
heroSpeed =
    8


{-| initialize the hero
-}
init : () -> Hero
init _ =
    Hero ( 500, 800 ) 3 False False False False None 0 3 0 False <|
        Animation 0 200 False False 0 0


{-| animate the hero for a new frame
-}
animateHero :
    Float
    -> { a | hero : Hero, heroBullets : List HeroBullet }
    -> { a | hero : Hero, heroBullets : List HeroBullet }
animateHero elapsed model =
    model
        |> moveHero
        |> animateShooting elapsed
        |> animateHeroBullets
        |> deactivateAtkDouble elapsed


deactivateAtkDouble : Float -> { a | hero : Hero, heroBullets : List HeroBullet } -> { a | hero : Hero, heroBullets : List HeroBullet }
deactivateAtkDouble elapsed model =
    let
        atkDoubled =
            model.hero.atkDoubled

        nElapsed =
            model.hero.spElapsed + elapsed
    in
    if atkDoubled == False then
        model

    else if nElapsed >= 5000 then
        { model
            | hero = updateElapsed (deAtkDouble model.hero) -5000
        }

    else
        { model | hero = updateElapsed model.hero nElapsed }


updateElapsed : Hero -> Float -> Hero
updateElapsed hero nElapsed =
    { hero | spElapsed = hero.spElapsed + nElapsed }


animateShooting :
    Float
    -> { a | hero : Hero, heroBullets : List HeroBullet }
    -> { a | hero : Hero, heroBullets : List HeroBullet }
animateShooting elapsed ({ hero, heroBullets } as model) =
    let
        updatedAnimation =
            updateAnimation hero.shootingAnimation elapsed

        newHero =
            { hero | shootingAnimation = updatedAnimation }
    in
    if updatedAnimation.shouldTrigger then
        { model | hero = newHero, heroBullets = shootBullet hero :: heroBullets }

    else
        { model | hero = newHero }


{-| draw the hero
-}
drawHero : Hero -> Svg msg
drawHero hero =
    Svg.svg
        []
        [ Svg.use
            [ SvgAttr.height <| String.fromInt 110
            , SvgAttr.width <| String.fromInt 55
            , SvgAttr.xlinkHref "assets/hero.svg#hero"
            , SvgAttr.x <| String.fromInt <| Tuple.first hero.pos - 5
            , SvgAttr.y <| String.fromInt <| Tuple.second hero.pos - 10
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


moveHero : { a | hero : Hero } -> { a | hero : Hero }
moveHero ({ hero } as model) =
    let
        ( x, y ) =
            hero.pos

        newHero =
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
    in
    if inBoundsDimensions newHero.pos ( heroWidth, heroHeight ) then
        { model | hero = newHero }

    else
        model


{-| let the hero start moving in a direction
-}
startMove : Hero -> Hero
startMove hero =
    { hero | heroDir = direction hero }


startShooting :
    { a | hero : Hero, heroBullets : List HeroBullet }
    -> { a | hero : Hero, heroBullets : List HeroBullet }
startShooting ({ hero, heroBullets } as model) =
    if not hero.shootingAnimation.isActive then
        let
            animation =
                hero.shootingAnimation

            newAnimation =
                { animation | isActive = True, elapsed = 0 }

            newHero =
                { hero | shootingAnimation = newAnimation }
        in
        { model | hero = newHero, heroBullets = shootBullet hero :: heroBullets }

    else
        model


{-| stop the shooting animation
-}
stopShooting : Hero -> Hero
stopShooting hero =
    let
        animation =
            hero.shootingAnimation

        newAnimation =
            { animation | isActive = False }
    in
    { hero | shootingAnimation = newAnimation }


{-| make the hero shoot a bullet
-}
shootBullet : Hero -> HeroBullet
shootBullet hero =
    HeroBullet hero.pos 0 -10


{-| animate the hero bullets for a new frame
-}
animateHeroBullets :
    { a | heroBullets : List HeroBullet }
    -> { a | heroBullets : List HeroBullet }
animateHeroBullets ({ heroBullets } as model) =
    let
        newBullets =
            heroBullets
                |> List.map animateHeroBullet
    in
    { model | heroBullets = newBullets }


animateHeroBullet : HeroBullet -> HeroBullet
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


{-| draw the hero's bullets
-}
drawHeroBullets : List HeroBullet -> List (Svg Msg)
drawHeroBullets heroBullets =
    heroBullets |> List.map drawHeroBullet


drawHeroBullet : HeroBullet -> Svg Msg
drawHeroBullet bullet =
    let
        ( x, y ) =
            bullet.posBullet
    in
    Svg.svg
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        ]
        [ Svg.use
            [ SvgAttr.width <| String.fromInt bulletWidth
            , SvgAttr.height <| String.fromInt bulletHeight
            , SvgAttr.xlinkHref "assets/hero_bullet.svg#hero_bullet"
            ]
            []
        ]


{-| return player superpower selection to the function that calls it
-}
selectSuperPower : Hero -> Int
selectSuperPower hero =
    hero.spSelection


{-| set superpower from player selection
-}
setSuperpower : Hero -> Int -> Hero
setSuperpower hero selection =
    { hero | spSelection = selection }


{-| reduce superpower instances by 1
-}
useSuperpower : Hero -> Hero
useSuperpower hero =
    { hero | spInstance = hero.spInstance - 1 }


{-| set the ATK of the hero to be doubled
-}
atkDouble : Hero -> Hero
atkDouble hero =
    { hero | atkDoubled = True }


deAtkDouble : Hero -> Hero
deAtkDouble hero =
    { hero | atkDoubled = False }
