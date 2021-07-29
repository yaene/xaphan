module Collision exposing (checkCollision)

import Enemy exposing (Enemy, EnemyBullet, EnemyType(..), enemyHeight, enemyWidth)
import Field exposing (Pos)
import Hero exposing (Hero, HeroBullet, heroHeight, heroWidth)
import List


isHeroHit : Hero -> List EnemyBullet -> Bool
isHeroHit hero bullets =
    bullets |> List.any (isBulletCollidingHero hero)


checkCollision :
    { a
        | enemies : List Enemy
        , hero : Hero
        , enemyBullets : List EnemyBullet
        , heroBullets : List HeroBullet
    }
    ->
        { a
            | enemies : List Enemy
            , hero : Hero
            , enemyBullets : List EnemyBullet
            , heroBullets : List HeroBullet
        }
checkCollision model =
    model
        |> collideBulletsEnemies
        |> collideBulletsHero


isBulletCollidingHero : Hero -> EnemyBullet -> Bool
isBulletCollidingHero { pos } { posBullet } =
    isColliding ( pos, ( heroWidth, heroHeight ) ) ( posBullet, ( Enemy.bulletWidth, Enemy.bulletHeight ) )


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


collideBulletsEnemies :
    { a | enemies : List Enemy, heroBullets : List HeroBullet }
    -> { a | enemies : List Enemy, heroBullets : List HeroBullet }
collideBulletsEnemies ({ enemies, heroBullets } as model) =
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
    { model | enemies = aliveEnemies, heroBullets = uncollidedBullets }


collideBulletsHero :
    { a | hero : Hero, enemyBullets : List EnemyBullet }
    -> { a | hero : Hero, enemyBullets : List EnemyBullet }
collideBulletsHero ({ hero, enemyBullets } as model) =
    if isHeroHit hero enemyBullets then
        let
            newHero =
                { hero | hp = hero.hp - 1 }

            newBullets =
                List.filter (not << isBulletCollidingHero hero) enemyBullets
        in
        { model | hero = newHero, enemyBullets = newBullets }

    else
        model


reduceEnemyHealth : Enemy -> List HeroBullet -> Enemy
reduceEnemyHealth enemy heroBullets =
    if isEnemyHit enemy heroBullets && enemy.enemyType /= Environmental then
        { enemy | hp = enemy.hp - 1 }

    else
        enemy
