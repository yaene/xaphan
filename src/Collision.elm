module Collision exposing (..)

import Enemy exposing (Enemy, EnemyBullet, bulletHeight, bulletWidth)
import Hero exposing (Hero, HeroBullet, heroHeight, heroWidth)


isHeroHit : Hero -> List EnemyBullet -> Bool
isHeroHit hero bullets =
    bullets |> List.any (isBulletCollidingHero hero)


isBulletCollidingHero : Hero -> EnemyBullet -> Bool
isBulletCollidingHero { pos } { posBullet } =
    let
        ( bx1, by1 ) =
            posBullet

        ( bx2, by2 ) =
            ( bx1 + bulletWidth, by1 + bulletHeight )

        ( x, y ) =
            pos

        ( x2, y2 ) =
            ( x + heroWidth, y + heroHeight )
    in
    (x < bx2 && x2 > bx1)
        && (y < by2 && y2 > by1)


isEnemyHit : Enemy -> List HeroBullet -> Bool
isEnemyHit enemy bullets =
    bullets |> List.any (isBulletCollidingEnemy enemy)
 

isBulletCollidingEnemy : Enemy -> HeroBullet -> Bool
isBulletCollidingEnemy { pos } { posBullet } =
    let
        ( bx1, by1 ) =
            posBullet

        ( bx2, by2 ) =
            ( bx1 + bulletWidth, by1 + bulletHeight )

        ( x, y ) =
            pos

        ( x2, y2 ) =
            ( x + heroWidth, y + heroHeight )
    in
    (x < bx2 && x2 > bx1)
        && (y < by2 && y2 > by1)
