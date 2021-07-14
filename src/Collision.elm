module Collision exposing (isHeroHit)

import Enemy exposing (EnemyBullet, bulletHeight, bulletWidth)
import Hero exposing (Hero, heroHeight, heroWidth)


isHeroHit : Hero -> List EnemyBullet -> Bool
isHeroHit hero bullets =
    bullets |> List.any (isBulletColliding hero)


isBulletColliding : Hero -> EnemyBullet -> Bool
isBulletColliding { x, y } { pos } =
    let
        ( bx1, by1 ) =
            pos

        ( bx2, by2 ) =
            ( bx1 + bulletWidth, by1 + bulletHeight )

        ( x2, y2 ) =
            ( x + heroWidth, y + heroHeight )
    in
    (x < bx2 && x2 > bx1)
        && (y < by2 && y2 > by1)
