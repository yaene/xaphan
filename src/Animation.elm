module Animation exposing (Animation, newAnimation, newAnimationWithSub, updateAnimation)

{-| contains all information for stepping through an animation
-}


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


{-| create a new animation with most common default values
-}
newAnimation : Float -> Animation
newAnimation interval =
    Animation 0 interval False True 0 Nothing


newAnimationWithSub : Float -> Float -> Int -> Animation
newAnimationWithSub mainInterval subInterval steps =
    Animation 0 mainInterval False True 0 (Just <| SubAnimation 0 subInterval False False 0 steps)


{-| step the animation through a frame
-}
updateAnimation : Animation -> Float -> Animation
updateAnimation animation elapsed =
    let
        newMainAnimation =
            updateAnimation_ animation elapsed
    in
    case animation.subAnimation of
        Nothing ->
            newMainAnimation

        Just sub ->
            updateAnimationWithSub newMainAnimation sub elapsed


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


updateAnimationWithSub : Animation -> SubAnimation -> Float -> Animation
updateAnimationWithSub newMainAnimation sub elapsed =
    if newMainAnimation.shouldTrigger then
        {- if main animation is triggered sub becomes active -}
        { newMainAnimation | subAnimation = Just { sub | isActive = True } }

    else if sub.isActive then
        { newMainAnimation | subAnimation = Just <| animateSub sub elapsed }

    else
        {- if neither main is triggered nor sub is active theres no need to do anything -}
        newMainAnimation


animateSub : SubAnimation -> Float -> SubAnimation
animateSub sub elapsed =
    {- when sub is active run sub animation for defined number of steps
       after that sub becomes inactive again
    -}
    let
        isFinished =
            sub.triggerCount == sub.steps

        newSub =
            updateAnimation_ sub elapsed
    in
    if isFinished then
        { newSub | isActive = False, shouldTrigger = False, triggerCount = 0 }

    else
        newSub
