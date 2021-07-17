module Collision exposing (collideBulletsEnemies, collideBulletsHero, isHeroHit)

import Enemy exposing (Enemy, EnemyBullet, enemyHeight, enemyWidth)
import Field exposing (Pos)
import Hero exposing (Hero, HeroBullet, heroHeight, heroWidth)
import List exposing (map)


isHeroHit : Hero -> List EnemyBullet -> Bool
isHeroHit hero bullets =
    bullets |> List.any (isBulletCollidingHero hero)


isBulletCollidingHero : Hero -> EnemyBullet -> Bool
isBulletCollidingHero { pos } { posBullet } =
    isColliding ( pos, ( enemyWidth, enemyHeight ) ) ( posBullet, ( Enemy.bulletWidth, Enemy.bulletHeight ) )


isColliding : ( Pos, ( Int, Int ) ) -> ( Pos, ( Int, Int ) ) -> Bool
isColliding ( ( x1, y1 ), ( w1, h1 ) ) ( ( x2, y2 ), ( w2, h2 ) ) =
    (x1 < x2 + w2 && x1 + w1 > x2)
        && (y1 < y2 + h2 && y1 + h1 > y2)


isEnemyHit : Enemy -> List HeroBullet -> Bool
isEnemyHit enemy bullets =
    bullets |> List.any (isBulletCollidingEnemy enemy)


isBulletCollidingEnemy : Enemy -> HeroBullet -> Bool
isBulletCollidingEnemy { pos } { posBullet } =
    isColliding ( pos, ( enemyWidth, enemyHeight ) ) ( posBullet, ( Hero.bulletWidth, Hero.bulletHeight ) )


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


collideBulletsHero : Hero -> List EnemyBullet -> ( Hero, List EnemyBullet )
collideBulletsHero hero bullets =
    if isHeroHit hero bullets then
        ( { hero | hp = hero.hp - 1 }, List.filter (not << isBulletCollidingHero hero) bullets )

    else
        ( hero, bullets )


reduceEnemyHealth : Enemy -> List HeroBullet -> Enemy
reduceEnemyHealth enemy heroBullets =
    if isEnemyHit enemy heroBullets then
        { enemy | hp = enemy.hp - 1 }

    else
        enemy
