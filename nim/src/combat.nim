# Copyright 2023 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

import types

type
  EnemyRecord* = object
    ## Used to store information about the enemy
    ship*: ShipRecord ## The enemy's ship
    accuracy*: Natural ## The enemy's bonus to accuracy
    distance*: int ## The distance to the enemy's ship in combat
    combatAi*: ShipCombatAi ## The type of enemy's AI
    evasion*: Natural ## The enemy's bonus to evasion
    loot*: Natural ## The amount of money looted from the enemy after win the fight with it
    perception*: Natural ## The enemy's bonus to perception
    harpoonDuration*: Natural ## How long in combat rounds the enemy's ship will be stopped by the player's harpoon
    guns*: seq[array[3, Natural]] ## The list of guns on the enemy's ship, 0 - gun index in ship modules list, 1 - gunner order, 2 - amount of shoots from the gun, value below zero means that gun shoot once per that amount of rounds

var
  harpoonDuration*: Natural = 0 ## How long in combat rounds the player's ship will be stopped by an enemy's harpoon
  enemy*: EnemyRecord = EnemyRecord(ship: ShipRecord(skyX: 1, skyY: 1)); ## The enemy information

# Temporary code for interfacing with Ada

proc getAdaHarpoonDuration(playerDuration, enemyDuration: cint) {.raises: [], tags: [], exportc.} =
  harpoonDuration = playerDuration
  enemy.harpoonDuration = enemyDuration
