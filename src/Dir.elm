module Dir exposing (Dir(..), generateRandomDirs)

import Random


{-| specifies a direction in the horizontal
-}
type Dir
    = Left
    | Right
    | None


{-| generate random dir commands for objects with a direction that trigger a given message
-}
generateRandomDirs : List ( Int, { a | dir : Dir } ) -> (( Int, Dir ) -> msg) -> Cmd msg
generateRandomDirs list msg =
    list
        |> List.map (\( index, val ) -> randomDirGenerator ( index, val ))
        |> List.map (Random.generate msg)
        |> Cmd.batch


randomDirGenerator : ( Int, { a | dir : Dir } ) -> Random.Generator ( Int, Dir )
randomDirGenerator ( index, val ) =
    let
        randomDir =
            case val.dir of
                Left ->
                    Random.uniform Right [ None ]

                Right ->
                    Random.uniform Left [ None ]

                None ->
                    Random.uniform Right [ Left ]
    in
    randomDir |> Random.andThen (\dir -> Random.constant ( index, dir ))
