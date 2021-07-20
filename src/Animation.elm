module Animation exposing (Animation, newAnimation, newAnimationWithSub, updateAnimationWithSub)


type alias Animation =
    { elapsed : Float
    , interval : Float
    , shouldTrigger : Bool
    , isActive : Bool
    , triggerCount : Int
    , subAnimation : Maybe SubAnimation
    }


type alias SubAnimation =
    { elapsed : Float
    , interval : Float
    , shouldTrigger : Bool
    , isActive : Bool
    , triggerCount : Int
    , steps : Int
    }


newAnimation : Float -> Animation
newAnimation interval =
    Animation 0 interval False True 0 Nothing


newAnimationWithSub : Float -> Float -> Int -> Animation
newAnimationWithSub mainInterval subInterval steps =
    Animation 0 mainInterval False True 0 (Just <| SubAnimation 0 subInterval False False 0 steps)


updateAnimation_ :
    { a | isActive : Bool, elapsed : Float, interval : Float, triggerCount : Int, shouldTrigger : Bool }
    -> Float
    -> { a | isActive : Bool, elapsed : Float, interval : Float, triggerCount : Int, shouldTrigger : Bool }
updateAnimation_ animation elapsed =
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
        { animation | shouldTrigger = False, elapsed = 0, triggerCount = 0 }


updateAnimationWithSub : Animation -> Float -> Animation
updateAnimationWithSub animation elapsed =
    let
        newMainAnimation =
            updateAnimation_ animation elapsed
    in
    case animation.subAnimation of
        Nothing ->
            newMainAnimation

        Just sub ->
            if newMainAnimation.shouldTrigger then
                { newMainAnimation | subAnimation = Just { sub | isActive = True } }

            else if sub.isActive then
                let
                    isFinished =
                        sub.triggerCount == sub.steps

                    newSub_ =
                        updateAnimation_ sub elapsed

                    newSub =
                        if isFinished then
                            { newSub_ | isActive = False, shouldTrigger = False, triggerCount = 0 }

                        else
                            newSub_
                in
                { newMainAnimation | subAnimation = Just newSub, shouldTrigger = newSub.shouldTrigger && newSub.isActive }

            else
                newMainAnimation
