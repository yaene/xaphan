module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Dir exposing (Dir(..))
import Hero
import Html
import Html.Attributes as HtmlAttr
import Html.Events exposing (keyCode)
import Json.Decode
import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


type alias Model =
    { hero : Hero.Hero
    , enemies : List Enemy
    , enemyBullets : List EnemyBullet
    }


type alias EnemyBullet =
    { pos : Pos, dx : Int, dy : Int }


type alias Pos =
    ( Int, Int )


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


type Msg
    = MoveHeroUp Bool
    | MoveHeroDown Bool
    | MoveHeroLeft Bool
    | MoveHeroRight Bool
    | Tick Float
    | ChangeEnemyDir ( Int, Dir )
    | Noop


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Hero.init ()) [ Enemy ( 50, 50 ) 3 0 Right 0 False 0 False ] [], Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div
        [ HtmlAttr.style "height" "100%"
        , HtmlAttr.style "display" "flex"
        , HtmlAttr.style "justify-content" "center"
        ]
        [ Html.div
            [ HtmlAttr.style "width" "50%"
            , HtmlAttr.style "background-color" "gray"
            ]
            [ Svg.svg
                [ SvgAttr.height "100%"
                , SvgAttr.width "100%"
                , SvgAttr.viewBox "0 0 1000 1000"
                ]
                (Hero.draw model.hero
                    :: (drawEnemies model
                            ++ drawBullets model
                       )
                )
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp (Json.Decode.map (key False) keyCode)
        , onKeyDown (Json.Decode.map (key True) keyCode)
        ]


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        -- key: arrow left
        37 ->
            MoveHeroLeft on

        -- key: a
        65 ->
            MoveHeroLeft on

        -- key: arrow right
        39 ->
            MoveHeroRight on

        -- key: d
        68 ->
            MoveHeroRight on

        -- key: arrow down
        40 ->
            MoveHeroDown on

        -- key: s
        83 ->
            MoveHeroDown on

        -- key: arrow up
        38 ->
            MoveHeroUp on

        -- key: w
        87 ->
            MoveHeroUp on

        _ ->
            Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        hero =
            model.hero
    in
    case msg of
        MoveHeroLeft on ->
            ( { model | hero = Hero.startMove { hero | moveLeft = on } }
            , Cmd.none
            )

        MoveHeroRight on ->
            ( { model | hero = Hero.startMove { hero | moveRight = on } }
            , Cmd.none
            )

        MoveHeroUp on ->
            ( { model | hero = Hero.startMove { hero | moveUp = on } }
            , Cmd.none
            )

        MoveHeroDown on ->
            ( { model | hero = Hero.startMove { hero | moveDown = on } }
            , Cmd.none
            )

        Tick elapsed ->
            model |> animate elapsed

        ChangeEnemyDir ( index, dir ) ->
            ( model |> changeEnemyDir index dir, Cmd.none )

        Noop ->
            ( model, Cmd.none )


animate : Float -> Model -> ( Model, Cmd Msg )
animate elapsed model =
    let
        nModel =
            { model | hero = Hero.moveHero model.hero }
    in
    nModel |> animateEnemies elapsed


drawEnemies : Model -> List (Svg Msg)
drawEnemies model =
    model.enemies |> List.map drawEnemy


drawBullets : Model -> List (Svg Msg)
drawBullets model =
    model.enemyBullets |> List.map drawBullet


drawBullet : EnemyBullet -> Svg Msg
drawBullet bullet =
    let
        ( x, y ) =
            bullet.pos
    in
    Svg.rect
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.fill "red"
        , SvgAttr.width "10"
        , SvgAttr.height "20"
        ]
        []


animateEnemies : Float -> Model -> ( Model, Cmd Msg )
animateEnemies elapsed model =
    let
        newEnemies =
            model.enemies |> List.map (animateEnemy elapsed)

        -- find enemies that should change direction and their index
        changeDirEnemies =
            newEnemies
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> .changeDir)

        shootBulletEnemies =
            newEnemies |> List.filter .shootBullet
    in
    ( { model
        | enemies = newEnemies
        , enemyBullets = animateEnemyBullets shootBulletEnemies model.enemyBullets
      }
    , Dir.generateRandomDirs changeDirEnemies ChangeEnemyDir
    )


animateEnemyBullets : List Enemy -> List EnemyBullet -> List EnemyBullet
animateEnemyBullets shooters bullets =
    bullets
        |> List.map animateEnemyBullet
        |> (++) (List.map shootBullet shooters)


shootBullet : Enemy -> EnemyBullet
shootBullet shooter =
    EnemyBullet shooter.pos 0 5


animateEnemyBullet bullet =
    let
        ( x, y ) =
            bullet.pos
    in
    { bullet | pos = ( x, y + bullet.dy ) }


changeEnemyDir : Int -> Dir -> Model -> Model
changeEnemyDir index dir model =
    let
        enemies =
            model.enemies
    in
    { model
        | enemies =
            enemies
                |> List.indexedMap
                    (\i enemy ->
                        if i == index then
                            { enemy | dir = dir }

                        else
                            enemy
                    )
    }


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

        enemyWidth =
            80
    in
    if changeDirElapsed_ > interval || enemy.changeDir then
        { enemy | changeDirElapsed = changeDirElapsed_ - interval, changeDir = True }

    else
        { enemy | changeDirElapsed = changeDirElapsed_ }


inBoundsX : Pos -> Int -> Bool
inBoundsX ( x, _ ) width =
    let
        maxX =
            1000
    in
    x >= 0 && x + width <= maxX


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
        if inBoundsX ( newX, y ) 80 then
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
    Svg.rect
        [ SvgAttr.x <| String.fromInt x
        , SvgAttr.y <| String.fromInt y
        , SvgAttr.fill "orange"
        , SvgAttr.width "80"
        , SvgAttr.height "100"
        ]
        []
