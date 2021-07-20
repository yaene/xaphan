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
    , newBasicEnemy
    , newSpiralEnemy
    , newSunEnemy
    )

import Animation exposing (Animation, newAnimation, updateAnimationWithSub)
import Dict exposing (update)
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
    , dir : Dir
    , changeDirAnimation : Animation
    , triggerShootAnimaton : Animation
    , shootBulletFunc : Int -> Pos -> List EnemyBullet
    }


type alias EnemyBullet =
    { posBullet : Pos, dx : Int, dy : Int }


newBasicEnemy : Pos -> Dir -> Enemy
newBasicEnemy pos dir =
    Enemy pos 5 dir (newAnimation 1500) (newAnimation 1000) shootBullet


newSunEnemy : Pos -> Dir -> Enemy
newSunEnemy pos dir =
    Enemy pos 5 dir (newAnimation 1500) (newAnimation 1000) shootSunBullets


newSpiralEnemy : Pos -> Dir -> Enemy
newSpiralEnemy pos dir =
    Enemy pos 5 dir (newAnimation 1500) (newAnimation 20) shootSpiralBullet


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
shootSunBullets _ shooterPos =
    List.range 0 7
        |> List.map
            (\i ->
                EnemyBullet (moveBy ( round <| enemyWidth / 2, round <| enemyHeight / 2 ) shooterPos)
                    (round <| (15 * cos ((i |> toFloat) * pi / 4)))
                    (round <| (15 * sin ((i |> toFloat) * pi / 4)))
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
animateShootBullet elapsed enemy =
    let
        newAnimation =
            updateAnimationWithSub enemy.triggerShootAnimaton elapsed

        bullets =
            if newAnimation.shouldTrigger then
                enemy.shootBulletFunc newAnimation.triggerCount enemy.pos

            else
                []
    in
    ( { enemy | triggerShootAnimaton = newAnimation }, bullets )


animateDirChange : Float -> Enemy -> Enemy
animateDirChange elapsed enemy =
    let
        newAnimation =
            updateAnimationWithSub enemy.changeDirAnimation elapsed
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
    rect
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.fill "orange"
        , SvgAttr.width <| String.fromInt enemyWidth
        , SvgAttr.height <| String.fromInt enemyHeight
        ]
        []
