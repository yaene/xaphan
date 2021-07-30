module Enemy exposing
    ( Enemy
    , EnemyBullet
    , EnemyType(..)
    , animateEnemies
    , bulletHeight
    , bulletWidth
    , changeDirCmds
    , changeEnemyDir
    , drawBullets
    , drawEnemies
    , enemyHeight
    , enemyWidth
    , finalBoss
    , newBasicEnemy
    , newEnvironmentalEnemy
    , newSpiralEnemy
    , newSunEnemy
    )

import Animation exposing (Animation, newAnimation, newAnimationWithDelay, updateAnimation)
import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsX, moveBy)
import Messages exposing (Msg(..))
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


{-| The hitbox width of the enemy
-}
enemyWidth : EnemyType -> number
enemyWidth enemyType =
    if enemyType == Final then
        100

    else
        80


{-| The hitbox height of the enemy
-}
enemyHeight : EnemyType -> number
enemyHeight enemyType =
    if enemyType == Final then
        160

    else
        80


{-| The hitbox width of the enemy bullet
-}
bulletWidth : number
bulletWidth =
    15


{-| The hitbox height of the enemy bullet
-}
bulletHeight : number
bulletHeight =
    25


{-| The different types of enemies.
Use in case differentiating between different types is needed
-}
type EnemyType
    = Basic
    | Sun
    | Final
    | Spiral
    | Environmental


type alias SubShootAnimation =
    { shootFunc : Int -> { pos : Pos, enemyType : EnemyType } -> List EnemyBullet
    , animation : Animation
    }


{-| Contains all data needed to animate an enemy.
-}
type alias Enemy =
    { pos : Pos
    , hp : Int
    , maxHp : Int
    , dir : Dir
    , changeDirAnimation : Animation
    , triggerShootAnimation : Animation
    , enemyType : EnemyType
    , subShootAnimation : Maybe SubShootAnimation
    }


{-| Contains all data needed to animate an enemy bullet
-}
type alias EnemyBullet =
    { pos : Pos, dx : Int, dy : Int }


bulletSpeed : Float
bulletSpeed =
    10


{-| Create a basic enemy that only shoots straight down
-}
newBasicEnemy : Pos -> Dir -> Enemy
newBasicEnemy pos dir =
    Enemy pos 5 5 dir (newAnimation 1500 0) (newAnimation 1000 0) Basic Nothing


{-| Create an enemy that shoots in a few directions at once
-}
newSunEnemy : Pos -> Dir -> Enemy
newSunEnemy pos dir =
    Enemy pos 5 5 dir (newAnimation 1500 0) (newAnimation 1000 0) Sun Nothing


{-| Create an "fake" enemy that shoots in circles and doesnt show on the screen.
Use to add shooting animation without an enemy.
-}
newEnvironmentalEnemy : Pos -> Float -> Enemy
newEnvironmentalEnemy pos delay =
    Enemy pos 1 1 None (newAnimation 0 -1) (newAnimationWithDelay delay 1000 0) Environmental Nothing


{-| Create an enemy that shoots in a Spiral
-}
newSpiralEnemy : Pos -> Dir -> Float -> Enemy
newSpiralEnemy pos dir startElapsed =
    Enemy pos
        5
        5
        dir
        (newAnimation 1500 0)
        (Animation startElapsed 1500 False True 0 0)
        Spiral
    <|
        Just <|
            SubShootAnimation shootSpiralBullet <|
                newAnimation 20 20


{-| Create a "final boss" that switches between different shooting patterns
-}
finalBoss : Pos -> Dir -> Enemy
finalBoss pos dir =
    Enemy pos 10 10 dir (newAnimation 1500 0) (newAnimation 1000 0) Final Nothing


{-| Draw all the enemies in a list
-}
drawEnemies : List Enemy -> List (Svg Msg)
drawEnemies enemies =
    enemies |> List.map drawEnemy


{-| Draw all the enemy bullets in a list
-}
drawBullets : List EnemyBullet -> List (Svg Msg)
drawBullets enemyBullets =
    enemyBullets |> List.map drawBullet


drawBullet : EnemyBullet -> Svg Msg
drawBullet bullet =
    let
        ( x, y ) =
            bullet.pos
    in
    Svg.svg
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        ]
        [ Svg.use
            [ SvgAttr.xlinkHref "assets/enemy_bullet.svg#enemy_bullet"
            , SvgAttr.width <| String.fromInt bulletWidth
            , SvgAttr.height <| String.fromInt bulletHeight
            ]
            []
        ]


{-| animate all the enemies in a list (Tick)
-}
animateEnemies :
    Float
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
animateEnemies elapsed model =
    model
        |> animateEnemies_ elapsed
        |> animateEnemyBullets


{-| generate random direction commands for all enemies that should change direction
-}
changeDirCmds : Float -> { a | enemies : List Enemy } -> ( { a | enemies : List Enemy }, Cmd Msg )
changeDirCmds elapsed ({ enemies } as model) =
    let
        newEnemies =
            enemies |> List.map (animateDirChange elapsed)

        changeDirEnemies =
            newEnemies
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> .changeDirAnimation >> .shouldTrigger)
    in
    ( { model | enemies = newEnemies }, Dir.generateRandomDirs changeDirEnemies ChangeEnemyDir )


animateEnemies_ :
    Float
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
animateEnemies_ elapsed model =
    let
        ( newEnemies, newBullets ) =
            model
                |> (.enemies >> List.map (animateEnemy elapsed))
                |> List.unzip
                |> Tuple.mapSecond List.concat
    in
    { model | enemyBullets = model.enemyBullets ++ newBullets, enemies = newEnemies }


animateEnemyBullets :
    { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
animateEnemyBullets model =
    let
        newBullets =
            model.enemyBullets
                |> List.map animateEnemyBullet
    in
    { model | enemyBullets = newBullets }


shootSunBullets : Int -> { a | pos : Pos, enemyType : EnemyType } -> List EnemyBullet
shootSunBullets offset { pos, enemyType } =
    List.range 0 7
        |> List.map
            (\i ->
                EnemyBullet (moveBy ( round <| enemyWidth enemyType / 2, round <| enemyHeight enemyType / 2 ) pos)
                    (round <| (bulletSpeed * cos ((i |> toFloat) * pi / 4 + (offset |> toFloat) * pi / 10)))
                    (round <| (bulletSpeed * sin ((i |> toFloat) * pi / 4 + (offset |> toFloat) * pi / 10)))
            )


shootCircleBullets : Int -> { a | pos : Pos, enemyType : EnemyType } -> List EnemyBullet
shootCircleBullets _ { pos, enemyType } =
    List.range 0 15
        |> List.map
            (\i ->
                EnemyBullet (moveBy ( round <| enemyWidth enemyType / 2, round <| enemyHeight enemyType / 2 ) pos)
                    (round <| (bulletSpeed * cos ((i |> toFloat) * pi / 8)))
                    (round <| (bulletSpeed * sin ((i |> toFloat) * pi / 8)))
            )


shootBullet : Int -> { a | pos : Pos, enemyType : EnemyType } -> List EnemyBullet
shootBullet _ { pos, enemyType } =
    [ EnemyBullet (moveBy ( round <| enemyWidth enemyType / 2, enemyHeight enemyType ) pos) 0 <| round bulletSpeed ]


shootSpiralBullet : Int -> { a | pos : Pos, enemyType : EnemyType } -> List EnemyBullet
shootSpiralBullet count { pos, enemyType } =
    [ EnemyBullet (moveBy ( round <| enemyWidth enemyType / 2, round <| enemyHeight enemyType / 2 ) pos)
        (round <| (bulletSpeed * cos ((count |> toFloat) * pi / 10)))
        (round <| (bulletSpeed * sin ((count |> toFloat) * pi / 10)))
    ]


animateEnemyBullet : EnemyBullet -> EnemyBullet
animateEnemyBullet bullet =
    let
        ( x, y ) =
            bullet.pos
    in
    { bullet | pos = ( x + bullet.dx, y + bullet.dy ) }


{-| change an enemies direction at an index
-}
changeEnemyDir : Int -> Dir -> List Enemy -> List Enemy
changeEnemyDir index dir enemies =
    enemies
        |> List.indexedMap
            (\i enemy ->
                if i == index then
                    { enemy | dir = dir }

                else
                    enemy
            )


animateEnemy : Float -> Enemy -> ( Enemy, List EnemyBullet )
animateEnemy elapsed enemy =
    enemy
        |> moveEnemy
        |> animateShootBullet elapsed


animateShootBullet : Float -> Enemy -> ( Enemy, List EnemyBullet )
animateShootBullet elapsed enemy_ =
    let
        newAnimation =
            updateAnimation enemy_.triggerShootAnimation elapsed

        enemy =
            { enemy_ | triggerShootAnimation = newAnimation }
    in
    case enemy.enemyType of
        Final ->
            animateFinalBoss elapsed enemy

        Spiral ->
            animateSpiralEnemy elapsed enemy

        Basic ->
            ( enemy, triggerShoot newAnimation <| shootBullet 0 enemy )

        Sun ->
            ( enemy, triggerShoot newAnimation <| shootSunBullets 0 enemy )

        Environmental ->
            ( enemy, triggerShoot newAnimation <| shootCircleBullets 0 enemy )


animateSpiralEnemy : Float -> Enemy -> ( Enemy, List EnemyBullet )
animateSpiralEnemy elapsed enemy =
    let
        mainAnimation =
            enemy.triggerShootAnimation
    in
    case enemy.subShootAnimation of
        Just subShootAnimation ->
            let
                sub =
                    subShootAnimation.animation

                newSub =
                    if sub.isActive then
                        updateAnimation sub elapsed

                    else if mainAnimation.shouldTrigger then
                        { sub | isActive = True }

                    else
                        sub

                newSubShoot =
                    { subShootAnimation | animation = newSub }

                newBullets =
                    triggerShoot sub (shootSpiralBullet sub.triggerCount enemy)
            in
            ( { enemy | subShootAnimation = Just newSubShoot }, newBullets )

        Nothing ->
            ( enemy, [] )


animateFinalBoss : Float -> Enemy -> ( Enemy, List EnemyBullet )
animateFinalBoss elapsed enemy_ =
    let
        { triggerShootAnimation } =
            enemy_

        enemy =
            if triggerShootAnimation.shouldTrigger then
                case triggerShootAnimation.triggerCount |> modBy 3 of
                    0 ->
                        { enemy_
                            | subShootAnimation =
                                Just <|
                                    SubShootAnimation shootSpiralBullet <|
                                        newAnimation 40 20
                        }

                    1 ->
                        { enemy_ | subShootAnimation = Just <| SubShootAnimation shootSunBullets <| newAnimation 200 4 }

                    2 ->
                        { enemy_ | subShootAnimation = Just <| SubShootAnimation shootCircleBullets <| newAnimation 100 8 }

                    _ ->
                        enemy_

            else
                enemy_
    in
    case enemy.subShootAnimation of
        Just { shootFunc, animation } ->
            let
                newSub =
                    updateAnimation animation elapsed

                newBullets =
                    triggerShoot animation
                        (shootFunc animation.triggerCount { pos = enemy.pos, enemyType = enemy.enemyType })
            in
            ( { enemy | subShootAnimation = Just <| SubShootAnimation shootFunc newSub }, newBullets )

        Nothing ->
            ( enemy, [] )


triggerShoot : Animation -> List EnemyBullet -> List EnemyBullet
triggerShoot animation shootFunc =
    if animation.shouldTrigger then
        shootFunc

    else
        []


animateDirChange : Float -> Enemy -> Enemy
animateDirChange elapsed enemy =
    let
        newAnimation =
            updateAnimation enemy.changeDirAnimation elapsed
    in
    { enemy | changeDirAnimation = newAnimation }


moveEnemy : Enemy -> Enemy
moveEnemy enemy =
    let
        ( x, y ) =
            enemy.pos

        dx =
            5

        dirFactor =
            case enemy.dir of
                Left ->
                    -1

                Right ->
                    1

                None ->
                    0

        newX =
            x + dirFactor * dx
    in
    if inBoundsX ( newX, y ) <| enemyWidth enemy.enemyType then
        { enemy | pos = ( newX, y ) }

    else
        { enemy | pos = ( x, y ) }


drawEnemy : Enemy -> Svg Msg
drawEnemy enemy =
    let
        ( x, y ) =
            enemy.pos

        display =
            if enemy.enemyType == Environmental then
                "none"

            else
                "inline"

        svgSrc =
            if enemy.enemyType == Final then
                "assets/boss.svg#boss"

            else
                "assets/monster.svg#monster"
    in
    Svg.svg
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.height <| String.fromInt <| enemyHeight enemy.enemyType + 5
        , SvgAttr.display display
        ]
        [ Svg.use
            [ SvgAttr.xlinkHref svgSrc
            , SvgAttr.width <| String.fromInt <| enemyWidth enemy.enemyType
            , SvgAttr.height <| String.fromInt <| enemyHeight enemy.enemyType
            , SvgAttr.y <| String.fromInt 5
            ]
            []
        , drawHpBar enemy
        ]


drawHpBar : Enemy -> Svg Msg
drawHpBar { hp, maxHp, enemyType } =
    let
        barWidth =
            enemyWidth enemyType // maxHp

        bars =
            List.range 0 (hp - 1)
                |> List.map
                    (\i ->
                        Svg.rect
                            [ SvgAttr.width <| String.fromInt barWidth
                            , SvgAttr.height <| String.fromInt 5
                            , SvgAttr.fill "orange"
                            , SvgAttr.x <| String.fromInt <| i * (barWidth + 1)
                            ]
                            []
                    )
    in
    Svg.svg [] bars
