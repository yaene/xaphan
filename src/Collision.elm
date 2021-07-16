module Collision exposing (collideBulletsEnemies, isHeroHit)

import Enemy exposing (Enemy, EnemyBullet, bulletHeight, bulletWidth)
import Hero exposing (Hero, HeroBullet, heroHeight, heroWidth)
import List exposing (map)


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


collideBulletsEnemies : List Enemy -> List HeroBullet -> ( List Enemy, List HeroBullet )
collideBulletsEnemies enemies heroBullets =
    let
        aliveEnemies =
            enemies
                |> List.map (\enemy -> reduceEnemyHealth enemy heroBullets)
                |> List.filter (.hp >> (<) 0)

        uncollidedBullets =
            heroBullets
                |> List.filter
                    (\bullet ->
                        List.all (not << (\enemy -> isBulletCollidingEnemy enemy bullet)) enemies
                    )
    in
    ( aliveEnemies, uncollidedBullets )


reduceEnemyHealth : Enemy -> List HeroBullet -> Enemy
reduceEnemyHealth enemy heroBullets =
    if isEnemyHit enemy heroBullets then
        { enemy | hp = enemy.hp - 1 }

    else
        enemy
