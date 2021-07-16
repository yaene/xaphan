module Enemy exposing (Enemy, EnemyBullet, animateEnemies, bulletHeight, bulletWidth, changeEnemyDir, drawBullets, drawEnemies)

import Dir exposing (Dir(..))
import Field exposing (Pos, inBoundsX, moveBy)
import Messages exposing (Msg(..))
import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


enemyWidth =
    80


enemyHeight =
    100


bulletWidth =
    10


bulletHeight =
    20


type alias Enemy =
    { pos : Pos
    , hp : Int
    , animationElapsed : Float
    , dir : Dir
    , changeDirElapsed : Float
    , changeDir : Bool
    , shootBulletElapsed : Float
    , shootBullet : Bool
    }


type alias EnemyBullet =
    { posBullet : Pos, dx : Int, dy : Int }


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


animateEnemies : Float -> ( List Enemy, List EnemyBullet ) -> ( List Enemy, List EnemyBullet, Cmd Msg )
animateEnemies elapsed ( enemies, enemyBullets ) =
    let
        newEnemies =
            enemies |> List.map (animateEnemy elapsed)

        -- find enemies that should change direction and their index
        changeDirEnemies =
            newEnemies
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> .changeDir)

        shootBulletEnemies =
            newEnemies |> List.filter .shootBullet
    in
    ( newEnemies
    , animateEnemyBullets shootBulletEnemies enemyBullets
    , Dir.generateRandomDirs changeDirEnemies ChangeEnemyDir
    )


animateEnemyBullets : List Enemy -> List EnemyBullet -> List EnemyBullet
animateEnemyBullets shooters bullets =
    bullets
        |> List.map animateEnemyBullet
        |> (++) (List.map shootBullet shooters)


shootBullet : Enemy -> EnemyBullet
shootBullet shooter =
    EnemyBullet (moveBy ( round <| enemyWidth / 2, enemyHeight ) shooter.pos) 0 5


animateEnemyBullet bullet =
    let
        ( x, y ) =
            bullet.posBullet
    in
    { bullet | posBullet = ( x, y + bullet.dy ) }


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


animateEnemy : Float -> Enemy -> Enemy
animateEnemy elapsed enemy =
    enemy
        |> moveEnemy elapsed
        |> animateDirChange elapsed
        |> animateShootBullet elapsed


animateShootBullet : Float -> Enemy -> Enemy
animateShootBullet elapsed enemy =
    let
        shootBulletElapsed =
            enemy.shootBulletElapsed + elapsed

        interval =
            1000
    in
    if shootBulletElapsed > interval then
        { enemy | shootBulletElapsed = shootBulletElapsed - interval, shootBullet = True }

    else
        { enemy | shootBulletElapsed = shootBulletElapsed, shootBullet = False }


animateDirChange : Float -> Enemy -> Enemy
animateDirChange elapsed enemy =
    let
        changeDirElapsed_ =
            enemy.changeDirElapsed + elapsed

        interval =
            2000
    in
    if changeDirElapsed_ > interval || enemy.changeDir then
        { enemy | changeDirElapsed = changeDirElapsed_ - interval, changeDir = True }

    else
        { enemy | changeDirElapsed = changeDirElapsed_ }


moveEnemy : Float -> Enemy -> Enemy
moveEnemy elapsed enemy =
    let
        ( x, y ) =
            enemy.pos

        dx =
            5

        interval =
            20

        animationElapsed_ =
            enemy.animationElapsed + elapsed

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
    if animationElapsed_ > interval then
        if inBoundsX ( newX, y ) enemyWidth then
            { enemy | pos = ( newX, y ), animationElapsed = animationElapsed_ - interval, changeDir = False }

        else
            { enemy | pos = ( x, y ), animationElapsed = animationElapsed_ - interval, changeDir = True }

    else
        { enemy | animationElapsed = animationElapsed_ }


drawEnemy : Enemy -> Svg Msg
drawEnemy enemy =
    let
        ( x, y ) =
            enemy.pos
    in
    rect
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.fill "orange"
        , SvgAttr.width <| String.fromInt enemyWidth
        , SvgAttr.height <| String.fromInt enemyHeight
        ]
        []
