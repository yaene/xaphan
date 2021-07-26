module Messages exposing (..)

import Dir exposing (Dir)


type Msg
    = Tick Float
    | MoveHeroUp Bool
    | MoveHeroDown Bool
    | MoveHeroLeft Bool
    | MoveHeroRight Bool
    | HeroShootBullet
    | HeroUseSuperpower
    | ChangeEnemyDir ( Int, Dir )
    | NextLevel
    | ShowControls
    | Selecting
    | SelectSuperpower1
    | SelectSuperpower2
    | Pause
    | Resume
    | Reset
    | Retry
    | Noop
