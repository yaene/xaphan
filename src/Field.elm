module Field exposing (Pos, inBoundsX)


type alias Pos =
    ( Int, Int )


inBoundsX : Pos -> Int -> Bool
inBoundsX ( x, _ ) width =
    let
        maxX =
            1000
    in
    x >= 0 && x + width <= maxX
