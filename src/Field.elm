module Field exposing (Pos, filterOutOfBounds, inBoundsDimensions, inBoundsX, moveBy)


type alias Pos =
    ( Int, Int )


fieldWidth : Int
fieldWidth =
    1000


fieldHeight : Int
fieldHeight =
    1000


inBoundsX : Pos -> Int -> Bool
inBoundsX ( x, _ ) width =
    x >= 0 && x + width <= fieldWidth


inBoundsDimensions : Pos -> Pos -> Bool
inBoundsDimensions (( x, y ) as pos) dimensions =
    inBounds pos && inBounds (Tuple.mapBoth ((+) x) ((+) y) dimensions)


inBounds : Pos -> Bool
inBounds ( x, y ) =
    {- only  checks for position not worrying about objects dimensions -}
    (x >= 0 && x <= fieldWidth)
        && (y >= 0 && y <= fieldHeight)


moveBy : Pos -> Pos -> Pos
moveBy ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


filterOutOfBounds : List { a | pos : Pos } -> List { a | pos : Pos }
filterOutOfBounds objects =
    objects
        |> List.filter (.pos >> inBounds)
