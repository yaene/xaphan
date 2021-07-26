module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Collision exposing (checkCollision)
import Dir exposing (Dir(..))
import Enemy exposing (Enemy, EnemyBullet, animateEnemies, changeDirCmds, changeEnemyDir, drawBullets, drawEnemies)
import Hero exposing (..)
import Html
import Html.Attributes as HtmlAttr
import Html.Events exposing (keyCode)
import Json.Decode
import Levels exposing (Level, loadLevel)
import Messages exposing (Msg(..))
import Modals exposing (ModalType(..), drawModal)
import Svg
import Svg.Attributes as SvgAttr


type alias Model =
    { hero : Hero.Hero
    , heroBullets : List HeroBullet
    , enemies : List Enemy
    , enemyBullets : List EnemyBullet
    , level : Level
    , state : State
    }


type State
    = Playing
    | Paused
    | Cleared
    | GameOver


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
    ( Model (Hero.init ()) [] (loadLevel 1) [] 1 Playing, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        content =
            case model.state of
                Playing ->
                    [ Svg.svg
                        [ SvgAttr.height "99%"
                        , SvgAttr.width "99%"
                        , SvgAttr.viewBox "0 0 1000 1000"
                        ]
                        (drawHero model.hero
                            :: (drawEnemies model.enemies
                                    ++ drawBullets model.enemyBullets
                                    ++ drawBullets model.heroBullets
                               )
                        )
                    ]

                Paused ->
                    [ drawModal PauseMenu ]

                Cleared ->
                    Levels.drawClearedLevel model.level

                GameOver ->
                    [ drawModal LostMessage ]
    in
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "justify-content" "center"
        ]
        [ Html.div
            [ HtmlAttr.style "width" "100vh"
            , HtmlAttr.style "height" "100vh"
            , HtmlAttr.style "background-color" "gray"
            ]
            content
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
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

        --key: x
        88 ->
            HeroUseSuperpower

        -- key: z
        90 ->
            if on then
                HeroShootBullet

            else
                Noop

        -- key: ESC
        27 ->
            Pause

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

        HeroShootBullet ->
            ( { model | heroBullets = shootBullet hero :: model.heroBullets }
            , Cmd.none
            )

        HeroUseSuperpower ->
            ( model |> useSuperPower, Cmd.none )

        Tick elapsed ->
            model |> animate elapsed

        ChangeEnemyDir ( index, dir ) ->
            ( { model | enemies = changeEnemyDir index dir model.enemies }, Cmd.none )

        NextLevel ->
            let
                ( newModel, _ ) =
                    init ()
            in
            ( { newModel | enemies = loadLevel <| model.level + 1, state = Playing, level = model.level + 1 }, Cmd.none )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Resume ->
            ( { model | state = Playing }, Cmd.none )

        Retry ->
            let
                ( newModel, _ ) =
                    init ()
            in
            ( { newModel | enemies = loadLevel <| model.level, level = model.level }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


animate : Float -> Model -> ( Model, Cmd Msg )
animate elapsed model =
    case model.state of
        Playing ->
            model
                |> animateEnemies elapsed
                |> animateHero elapsed
                |> checkCollision
                |> newState
                |> changeDirCmds elapsed

        _ ->
            ( model, Cmd.none )


newState : Model -> Model
newState model =
    if model.hero.hp <= 0 then
        { model | state = GameOver }

    else if List.isEmpty model.enemies then
        case model.state of
            Playing ->
                { model | state = Cleared }

            _ ->
                model

    else
        model


useSuperPower : Model -> Model
useSuperPower model =
    let
        selection =
            selectSuperPower model.hero
    in
    case selection of
        1 ->
            { model | enemyBullets = spClearBullets model.enemyBullets model.hero }

        2 ->
            model

        3 ->
            model

        _ ->
            model


spClearBullets : List EnemyBullet -> Hero -> List EnemyBullet
spClearBullets enemyBullets hero =
    []
