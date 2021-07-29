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
    | HeroShootBullet
    | ChangeEnemyDir ( Int, Dir )
    | NextLevel
    | ShowControls
    | Pause
    | Resume
    | Reset
    | Retry
    | Noop
