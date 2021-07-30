module Messages exposing (..)

import Dir exposing (Dir)


{-| specifies all types of messages in the program
-}
type Msg
    = Tick Float
    | MoveHeroUp Bool
    | MoveHeroDown Bool
    | MoveHeroLeft Bool
    | MoveHeroRight Bool
    | HeroShootBullet Bool
    | HeroUseSuperpower
    | ChangeEnemyDir ( Int, Dir )
    | NextLevel
    | ShowControls
    | Selecting
    | SelectSuperpower Int
    | Pause
    | Resume
    | Reset
    | Retry
    | Noop
