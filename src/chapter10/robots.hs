
--- Fighting robot modelling
module Robots where

robot (name, attack, hp) = \message -> message (name, attack, hp)
printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))
damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n,a,h - attackDamage))
fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
          then getAttack aRobot
          else 0
life manyRobots = map (\aRobot -> (getHP aRobot)) manyRobots

name (n, _, _) = n
getName aRobot = aRobot name
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

attack (_, a, _) = a
getAttack aRobot = aRobot attack
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

hp (_, _, hp) = hp
getHP aRobot = aRobot hp
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

killerRobot = robot ("Kill3r", 25, 200)

threeVs1 attacker defenders = map attackerFight defenders
  where attackerFight = fight attacker

showThreeVs1Result fights = map printRobot fights
