module Field exposing (Pos, inBoundsX, moveBy)

{-| specifies a position on the playing field
-}


type alias Pos =
    ( Int, Int )


{-| determine whether an object with some width is inside the playing field
-}
inBoundsX : Pos -> Int -> Bool
inBoundsX ( x, _ ) width =
    let
        maxX =
            1000
    in
    x >= 0 && x + width <= maxX


{-| move a position by some change in x and y
-}
moveBy : Pos -> Pos -> Pos
moveBy ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )
