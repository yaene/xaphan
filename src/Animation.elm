module Animation exposing (..)


type alias Animation =
    { elapsed : Float
    , interval : Float
    , shouldTrigger : Bool
    , isActive : Bool
    }


updateAnimation : Animation -> Float -> Animation
updateAnimation animation elapsed =
    if animation.isActive then
        let
            nElapsed =
                animation.elapsed + elapsed
        in
        if nElapsed > animation.interval then
            { animation | elapsed = nElapsed - animation.interval, shouldTrigger = True }

        else
            { animation | elapsed = nElapsed, shouldTrigger = False }

    else
        { animation | shouldTrigger = False, elapsed = 0 }
