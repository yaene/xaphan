module Field exposing (Pos, filterOutOfBounds, inBoundsDimensions, inBoundsX, moveBy)

{-| specifies a position on the playing field
-}


type alias Pos =
    ( Int, Int )


fieldWidth : Int
fieldWidth =
    1000


fieldHeight : Int
fieldHeight =
    1000


{-| determine whether an object with some width is inside the playing field
-}
inBoundsX : Pos -> Int -> Bool
inBoundsX ( x, _ ) width =
    x >= 0 && x + width <= fieldWidth


{-| determine whether an object with some dimensions is inside the playing field
-}
inBoundsDimensions : Pos -> Pos -> Bool
inBoundsDimensions (( x, y ) as pos) dimensions =
    inBounds pos && inBounds (Tuple.mapBoth ((+) x) ((+) y) dimensions)


inBounds : Pos -> Bool
inBounds ( x, y ) =
    {- only  checks for position not worrying about objects dimensions -}
    (x >= 0 && x <= fieldWidth)
        && (y >= 0 && y <= fieldHeight)


{-| move a position by some change in x and y
-}
moveBy : Pos -> Pos -> Pos
moveBy ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


{-| filter away objects that are out of bounds
-}
filterOutOfBounds : List { a | pos : Pos } -> List { a | pos : Pos }
filterOutOfBounds objects =
    objects
        |> List.filter (.pos >> inBounds)
