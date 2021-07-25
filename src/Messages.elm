module Messages exposing (..)

import Dir exposing (Dir)


type Msg
    = Tick Float
    | MoveHeroUp Bool
    | MoveHeroDown Bool
    | MoveHeroLeft Bool
    | MoveHeroRight Bool
    | HeroShootBullet
    | ChangeEnemyDir ( Int, Dir )
    | NextLevel
    | ShowControls
    | Pause
    | Resume
    | Reset
    | Retry
    | Noop
