module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Collision exposing (collideBulletsEnemies, isHeroHit)
import Dir exposing (Dir(..))
import Enemy exposing (Enemy, EnemyBullet, animateEnemies, changeEnemyDir, drawBullets, drawEnemies)
import Field exposing (Pos)
import Hero exposing (..)
import Html
import Html.Attributes as HtmlAttr
import Html.Events exposing (keyCode)
import Json.Decode
import Messages exposing (Msg(..))
import Svg exposing (Svg, rect)
import Svg.Attributes as SvgAttr


type alias Model =
    { hero : Hero.Hero
    , heroBullets : List HeroBullet
    , enemies : List Enemy
    , enemyBullets : List EnemyBullet
    , state : State
    }


type State
    = Playing
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
    ( Model (Hero.init ()) [] [ Enemy ( 50, 50 ) 2 0 Right 0 False 0 False ] [] Playing, Cmd.none )


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
                (drawHero model.hero
                    :: (drawEnemies model.enemies
                            ++ drawBullets model.enemyBullets
                            ++ drawBullets model.heroBullets
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

        -- key: z
        90 ->
            if on then
                HeroShootBullet

            else
                Noop

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

        Tick elapsed ->
            model |> animate elapsed

        ChangeEnemyDir ( index, dir ) ->
            ( { model | enemies = changeEnemyDir index dir model.enemies }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


animate : Float -> Model -> ( Model, Cmd Msg )
animate elapsed model =
    if model.state == Playing then
        let
            ( enemies, enemyBullets, cmd ) =
                animateEnemies elapsed ( model.enemies, model.enemyBullets )

            ( hero, heroBullets ) =
                animateHero elapsed model.hero model.heroBullets

            ( aliveEnemies, uncollidedBullets ) =
                collideBulletsEnemies enemies heroBullets
        in
        ( { model
            | hero = hero
            , heroBullets = uncollidedBullets
            , enemies = aliveEnemies
            , enemyBullets = enemyBullets
            , state =
                model
                    |> newState
          }
        , cmd
        )

    else
        ( model, Cmd.none )


newState : Model -> State
newState model =
    if isHeroHit model.hero model.enemyBullets then
        GameOver

    else
        Playing
