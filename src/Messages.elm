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
    | Pause
    | Resume
    | Retry
    | Noop
