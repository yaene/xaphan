module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Collision exposing (checkCollision)
import Dir exposing (Dir(..))
import Enemy exposing (Enemy, EnemyBullet, EnemyType(..), animateEnemies, changeDirCmds, changeEnemyDir, drawBullets, drawEnemies)
import Field exposing (filterOutOfBounds)
import Hero exposing (..)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (keyCode)
import Json.Decode
import Levels exposing (Level, loadLevel)
import Messages exposing (Msg(..))
import Modals exposing (ModalType(..), drawModal)
import StyledComponents
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
    = Initial
    | Playing
    | Paused
    | Cleared
    | Controls
    | GameOver


{-| the game's main program
-}
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
    ( Model (Hero.init ()) [] [] [] 0 Initial, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        backgroundUrl =
            if model.state == Initial then
                "url(assets/Initial_Page.jpg)"

            else
                "url(assets/background.jpg)"

        content =
            case model.state of
                Initial ->
                    [ drawInitialPage ]

                Playing ->
                    [ Svg.svg
                        [ SvgAttr.height "99%"
                        , SvgAttr.width "99%"
                        , SvgAttr.viewBox "0 0 1000 1000"
                        ]
                        (drawHero model.hero
                            :: (drawEnemies model.enemies
                                    ++ drawBullets model.enemyBullets
                                    ++ drawHeroBullets model.heroBullets
                               )
                        )
                    ]

                Paused ->
                    [ drawModal PauseMenu ]

                Cleared ->
                    Levels.drawClearedLevel model.level

                GameOver ->
                    [ drawModal LostMessage ]

                Controls ->
                    [ drawModal ControlsInfo ]
    in
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "justify-content" "center"
        , HtmlAttr.style "background-color" "black"
        ]
        [ Html.div
            [ HtmlAttr.style "display" "flex"
            , HtmlAttr.style "width" "100vh"
            , HtmlAttr.style "height" "100vh"
            , HtmlAttr.style "background-image" backgroundUrl
            , HtmlAttr.style "background-size" "contain"
            , HtmlAttr.style "background-repeat" "no-repeat"
            , HtmlAttr.style "background-position" "center"
            , HtmlAttr.style "justify-content" "center"
            ]
            content
        ]


drawInitialPage : Html Msg
drawInitialPage =
    div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-direction" "column"
        , HtmlAttr.style "align-self" "center"
        , HtmlAttr.style "width" "250px"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "justify-content" "space-evenly"
        ]
        [ StyledComponents.button "New Game" NextLevel [ HtmlAttr.style "margin-bottom" "20px" ]
        , StyledComponents.button "Controls" ShowControls []
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

        -- key: z
        90 ->
            HeroShootBullet on

        -- key: SPACE
        32 ->
            HeroShootBullet on

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

        HeroShootBullet isActive ->
            if isActive then
                ( Hero.startShooting model
                , Cmd.none
                )

            else
                ( { model | hero = Hero.stopShooting hero }, Cmd.none )

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
            if model.state == Playing then
                ( { model | state = Paused }, Cmd.none )

            else
                ( model, Cmd.none )

        Resume ->
            ( { model | state = Playing }, Cmd.none )

        Reset ->
            init ()

        Retry ->
            let
                ( newModel, _ ) =
                    init ()
            in
            ( { newModel | enemies = loadLevel <| model.level, level = model.level, state = Playing }, Cmd.none )

        ShowControls ->
            ( { model | state = Controls }, Cmd.none )

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
                |> filterBulletsOutOfBounds
                |> newState
                |> changeDirCmds elapsed

        _ ->
            ( model, Cmd.none )


filterBulletsOutOfBounds : Model -> Model
filterBulletsOutOfBounds model =
    let
        filteredBullets =
            model |> (.enemyBullets >> filterOutOfBounds)

        filteredHeroBullets =
            model |> (.heroBullets >> filterOutOfBounds)
    in
    { model | enemyBullets = filteredBullets, heroBullets = filteredHeroBullets }


newState : Model -> Model
newState model =
    if model.hero.hp <= 0 then
        { model | state = GameOver }

    else if isLevelCleared model.enemies then
        case model.state of
            Playing ->
                { model | state = Cleared }

            _ ->
                model

    else
        model


isLevelCleared : List Enemy -> Bool
isLevelCleared enemies =
    enemies
        |> (List.filter <| .enemyType >> (/=) Environmental)
        |> List.isEmpty
