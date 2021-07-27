module Animation exposing (Animation, newAnimation, updateAnimation)


type alias Animation =
    { elapsed : Float
    , interval : Float
    , shouldTrigger : Bool
    , isActive : Bool
    , triggerCount : Int
    , steps : Int
    }


newAnimation : Float -> Int -> Animation
newAnimation interval steps =
    Animation 0 interval False True 0 steps


updateAnimation : Animation -> Float -> Animation
updateAnimation animation elapsed =
    if not animation.isActive || (animation.steps /= 0 && animation.triggerCount >= animation.steps) then
        { animation | shouldTrigger = False, elapsed = 0, triggerCount = 0, isActive = False }

    else
        let
            nElapsed =
                animation.elapsed + elapsed
        in
        if nElapsed > animation.interval then
            { animation | elapsed = nElapsed - animation.interval, shouldTrigger = True, triggerCount = animation.triggerCount + 1 }

        else
            { animation | elapsed = nElapsed, shouldTrigger = False }
