module Messages exposing (..)

import Dir exposing (Dir)


type Msg
    = Tick Float
    | MoveHeroUp Bool
    | MoveHeroDown Bool
    | MoveHeroLeft Bool
    | MoveHeroRight Bool
    | HeroShootBullet Bool
    | ChangeEnemyDir ( Int, Dir )
    | Noop
