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

import Animation exposing (Animation, newAnimation, updateAnimation)
import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsX, moveBy)
import Hero exposing (shootBullet)
import Messages exposing (Msg(..))
import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


enemyWidth =
    80


enemyHeight =
    80


bulletWidth =
    10


bulletHeight =
    20


type EnemyType
    = Basic
    | Sun
    | Final
    | Spiral
    | Environmental


type alias SubShootAnimation =
    { shootFunc : Int -> Pos -> List EnemyBullet
    , animation : Animation
    }


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


type alias EnemyBullet =
    { posBullet : Pos, dx : Int, dy : Int }


newBasicEnemy : Pos -> Dir -> Enemy
newBasicEnemy pos dir =
    Enemy pos 5 5 dir (newAnimation 1500 0) (newAnimation 1000 0) Basic Nothing


newSunEnemy : Pos -> Dir -> Enemy
newSunEnemy pos dir =
    Enemy pos 5 5 dir (newAnimation 1500 0) (newAnimation 1000 0) Sun Nothing


newEnvironmentalEnemy : Pos -> Enemy
newEnvironmentalEnemy pos =
    Enemy pos 1 1 None (newAnimation 0 -1) (newAnimation 800 0) Environmental Nothing


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


finalBoss : Pos -> Dir -> Enemy
finalBoss pos dir =
    Enemy pos 10 10 dir (newAnimation 1500 0) (newAnimation 1000 0) Final Nothing


finalBossShootBullet : Int -> Pos -> List EnemyBullet
finalBossShootBullet triggerCount enemy =
    shootSpiralBullet triggerCount enemy


drawEnemies : List Enemy -> List (Svg Msg)
drawEnemies enemies =
    enemies |> List.map drawEnemy


drawBullets : List EnemyBullet -> List (Svg Msg)
drawBullets enemyBullets =
    enemyBullets |> List.map drawBullet


drawBullet : EnemyBullet -> Svg Msg
drawBullet bullet =
    let
        ( x, y ) =
            bullet.posBullet
    in
    Svg.rect
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.fill "red"
        , SvgAttr.width <| String.fromInt bulletWidth
        , SvgAttr.height <| String.fromInt bulletHeight
        ]
        []


animateEnemies :
    Float
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
    -> { a | enemies : List Enemy, enemyBullets : List EnemyBullet }
animateEnemies elapsed model =
    model
        |> animateEnemies_ elapsed
        |> animateEnemyBullets


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


shootSunBullets : Int -> Pos -> List EnemyBullet
shootSunBullets offset shooterPos =
    List.range 0 7
        |> List.map
            (\i ->
                EnemyBullet (moveBy ( round <| enemyWidth / 2, round <| enemyHeight / 2 ) shooterPos)
                    (round <| (15 * cos ((i |> toFloat) * pi / 4 + (offset |> toFloat) * pi / 10)))
                    (round <| (15 * sin ((i |> toFloat) * pi / 4 + (offset |> toFloat) * pi / 10)))
            )


shootCircleBullets : Int -> Pos -> List EnemyBullet
shootCircleBullets _ shooterPos =
    List.range 0 15
        |> List.map
            (\i ->
                EnemyBullet (moveBy ( round <| enemyWidth / 2, round <| enemyHeight / 2 ) shooterPos)
                    (round <| (15 * cos ((i |> toFloat) * pi / 8)))
                    (round <| (15 * sin ((i |> toFloat) * pi / 8)))
            )


shootBullet : Int -> Pos -> List EnemyBullet
shootBullet _ shooterPos =
    [ EnemyBullet (moveBy ( round <| enemyWidth / 2, enemyHeight ) shooterPos) 0 5 ]


shootSpiralBullet : Int -> Pos -> List EnemyBullet
shootSpiralBullet count shooterPos =
    [ EnemyBullet (moveBy ( round <| enemyWidth / 2, round <| enemyHeight / 2 ) shooterPos)
        (round <| (15 * cos ((count |> toFloat) * pi / 10)))
        (round <| (15 * sin ((count |> toFloat) * pi / 10)))
    ]


animateEnemyBullet : EnemyBullet -> EnemyBullet
animateEnemyBullet bullet =
    let
        ( x, y ) =
            bullet.posBullet
    in
    { bullet | posBullet = ( x + bullet.dx, y + bullet.dy ) }


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
            ( enemy, triggerShoot newAnimation enemy.pos <| shootBullet 0 )

        Sun ->
            ( enemy, triggerShoot newAnimation enemy.pos <| shootSunBullets 0 )

        Environmental ->
            ( enemy, triggerShoot newAnimation enemy.pos <| shootCircleBullets 0 )


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
                    triggerShoot sub enemy.pos (shootSpiralBullet sub.triggerCount)
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
                    triggerShoot animation enemy.pos (shootFunc animation.triggerCount)
            in
            ( { enemy | subShootAnimation = Just <| SubShootAnimation shootFunc newSub }, newBullets )

        Nothing ->
            ( enemy, [] )


triggerShoot : Animation -> Pos -> (Pos -> List EnemyBullet) -> List EnemyBullet
triggerShoot animation pos shootFunc =
    if animation.shouldTrigger then
        shootFunc pos

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
    if inBoundsX ( newX, y ) enemyWidth then
        { enemy | pos = ( newX, y ) }

    else
        { enemy | pos = ( x, y ) }


drawEnemy : Enemy -> Svg Msg
drawEnemy enemy =
    let
        ( x, y ) =
            enemy.pos
    in
    Svg.svg
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.height <| String.fromInt <| enemyHeight + 5
        ]
        [ Svg.use
            [ SvgAttr.xlinkHref "assets/monster.svg#monster"
            , SvgAttr.width <| String.fromInt enemyWidth
            , SvgAttr.height <| String.fromInt enemyHeight
            , SvgAttr.y <| String.fromInt 5
            ]
            []
        , drawHpBar enemy.hp enemy.maxHp
        ]


drawHpBar : Int -> Int -> Svg Msg
drawHpBar hp maxHp =
    let
        barWidth =
            enemyWidth // maxHp

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
