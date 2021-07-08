module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Hero
import Html
import Html.Attributes as HtmlAttr
import Html.Events exposing (keyCode)
import Json.Decode
import Random
import Svg exposing (rect)
import Svg.Attributes as SvgAttr


type alias Model =
    { hero : Hero.Hero
    , enemies : List Enemy
    }


type alias Pos =
    ( Int, Int )


type Dir
    = Left
    | Right
    | None


type alias Enemy =
    { pos : Pos
    , hp : Int
    , animationElapsed : Float
    , dir : Dir
    , changeDirElapsed : Float
    , changeDir : Bool
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
    ( Model (Hero.init ()) [ Enemy ( 50, 50 ) 3 0 Right 0 False ], Cmd.none )


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
                    :: drawEnemies model
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


drawEnemies model =
    model.enemies |> List.map drawEnemy


animateEnemies : Float -> Model -> ( Model, Cmd Msg )
animateEnemies elapsed model =
    ( { model
        | enemies =
            model.enemies |> List.map (animateEnemy elapsed)
      }
    , generateRandomEnemyDirs model.enemies
    )


generateRandomEnemyDirs : List Enemy -> Cmd Msg
generateRandomEnemyDirs enemies =
    enemies
        |> List.indexedMap (\index enemy -> randomDirGenerator index enemy)
        |> List.filterMap identity
        |> List.map (Random.generate ChangeEnemyDir)
        |> Cmd.batch


randomDirGenerator : Int -> Enemy -> Maybe (Random.Generator ( Int, Dir ))
randomDirGenerator index enemy =
    if enemy.changeDir then
        let
            randomDir =
                case enemy.dir of
                    Left ->
                        Random.uniform Right [ None ]

                    Right ->
                        Random.uniform Left [ None ]

                    None ->
                        Random.uniform Right [ Left ]
        in
        Just (randomDir |> Random.andThen (\dir -> Random.constant ( index, dir )))

    else
        Nothing


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


animateDirChange elapsed enemy =
    let
        changeDirElapsed_ =
            enemy.changeDirElapsed + elapsed

        interval =
            2000
    in
    if changeDirElapsed_ > interval then
        { enemy | changeDirElapsed = changeDirElapsed_ - interval, changeDir = True }

    else
        { enemy | changeDirElapsed = changeDirElapsed_, changeDir = False }


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
    in
    if animationElapsed_ > interval then
        { enemy | pos = ( x + dirFactor * dx, y ), animationElapsed = animationElapsed_ - interval }

    else
        { enemy | animationElapsed = animationElapsed_ }


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
