module Enemy exposing
    ( Enemy
    , EnemyBullet
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
    , newSpiralEnemy
    , newSunEnemy
    )

import Animation exposing (Animation, newAnimation, updateAnimation)
import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsX, moveBy)
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


type alias Enemy =
    { pos : Pos
    , hp : Int
    , dir : Dir
    , changeDirAnimation : Animation
    , triggerShootAnimation : Animation
    , enemyType : EnemyType
    , subShootAnimation : Maybe Animation
    }


type alias EnemyBullet =
    { posBullet : Pos, dx : Int, dy : Int }


newBasicEnemy : Pos -> Dir -> Enemy
newBasicEnemy pos dir =
    Enemy pos 5 dir (newAnimation 1500 0) (newAnimation 1000 0) Basic Nothing


newSunEnemy : Pos -> Dir -> Enemy
newSunEnemy pos dir =
    Enemy pos 5 dir (newAnimation 1500 0) (newAnimation 1000 0) Sun Nothing


newSpiralEnemy : Pos -> Dir -> Float -> Enemy
newSpiralEnemy pos dir startElapsed =
    Enemy pos
        5
        dir
        (newAnimation 1500 0)
        (Animation startElapsed 1500 False True 0 0)
        Spiral
    <|
        Just <|
            newAnimation 20 20


finalBoss : Pos -> Dir -> Enemy
finalBoss pos dir =
    Enemy pos 10 dir (newAnimation 1500 0) (newAnimation 1000 0) Final Nothing


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


shootSunBullets : Pos -> List EnemyBullet
shootSunBullets shooterPos =
    List.range 0 7
        |> List.map
            (\i ->
                EnemyBullet (moveBy ( round <| enemyWidth / 2, round <| enemyHeight / 2 ) shooterPos)
                    (round <| (15 * cos ((i |> toFloat) * pi / 4)))
                    (round <| (15 * sin ((i |> toFloat) * pi / 4)))
            )


shootBullet : Pos -> List EnemyBullet
shootBullet shooterPos =
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
            ( enemy, triggerShoot newAnimation enemy.pos shootBullet )

        Sun ->
            ( enemy, triggerShoot newAnimation enemy.pos shootSunBullets )


animateSpiralEnemy : Float -> Enemy -> ( Enemy, List EnemyBullet )
animateSpiralEnemy elapsed enemy =
    let
        mainAnimation =
            enemy.triggerShootAnimation
    in
    case enemy.subShootAnimation of
        Just sub ->
            let
                newSub =
                    if sub.isActive then
                        updateAnimation sub elapsed

                    else if mainAnimation.shouldTrigger then
                        { sub | isActive = True }

                    else
                        sub

                newBullets =
                    triggerShoot sub enemy.pos (shootSpiralBullet sub.triggerCount)
            in
            ( { enemy | subShootAnimation = Just newSub }, newBullets )

        Nothing ->
            ( enemy, [] )


animateFinalBoss : Float -> Enemy -> ( Enemy, List EnemyBullet )
animateFinalBoss elapsed enemy_ =
    let
        { triggerShootAnimation } =
            enemy_

        enemy =
            if triggerShootAnimation.shouldTrigger then
                case triggerShootAnimation.triggerCount |> modBy 2 of
                    0 ->
                        { enemy_ | subShootAnimation = Just <| newAnimation 20 20 }

                    1 ->
                        { enemy_ | subShootAnimation = Nothing }

                    _ ->
                        enemy_

            else
                enemy_
    in
    case enemy.subShootAnimation of
        Just sub ->
            let
                newSub =
                    updateAnimation sub elapsed

                newBullets =
                    triggerShoot sub enemy.pos (shootSpiralBullet sub.triggerCount)
            in
            ( { enemy | subShootAnimation = Just newSub }, newBullets )

        Nothing ->
            ( enemy, triggerShoot triggerShootAnimation enemy.pos shootSunBullets )


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
    Svg.use
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.xlinkHref "assets/monster.svg#monster"
        , SvgAttr.width <| String.fromInt enemyWidth
        , SvgAttr.height <| String.fromInt enemyHeight
        ]
        []
