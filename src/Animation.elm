module Animation exposing (Animation, newAnimation, updateAnimation)


type alias Animation =
    { elapsed : Float
    , interval : Float
    , shouldTrigger : Bool
    , isActive : Bool
    , triggerCount : Int
    }


newAnimation : Float -> Animation
newAnimation interval =
    Animation 0 interval False True 0


updateAnimation : Animation -> Float -> Animation
updateAnimation animation elapsed =
    if animation.isActive then
        let
            nElapsed =
                animation.elapsed + elapsed
        in
        if nElapsed > animation.interval then
            { animation | elapsed = nElapsed - animation.interval, shouldTrigger = True, triggerCount = animation.triggerCount + 1 }

        else
            { animation | elapsed = nElapsed, shouldTrigger = False }

    else
        { animation | shouldTrigger = False, elapsed = 0 }
