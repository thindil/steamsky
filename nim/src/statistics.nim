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
  StatisticsData = object
    ## Used to store detailed information about some the player's game's
    ## statistics
    index: string ## The index of the prototype object
    amount: Positive ## The amount of the object

  GameStatsData = object
    ## Used to store information about the player's game's statistics
    destroyedShips: seq[StatisticsData]   ## The list of destroyed ships
    basesVisited: BasesRange              ## The amount of visited bases
    mapVisited: Positive                  ## The amount of visited map fields
    distanceTraveled: Natural             ## The length of the traveled distance
    craftingOrders: seq[StatisticsData]   ## The list of finished crafting orders
    acceptedMissions: Natural             ## The amount of accepted missions
    finishedMissions: seq[StatisticsData] ## The list of finished missions
    finishedGoals: seq[StatisticsData]    ## The list of finished goals
    killedMobs: seq[StatisticsData]       ## The list of enemies killed
    points: Natural                       ## The amount of points gained in the game's session

var gameStats* = GameStatsData(basesVisited: 1, mapVisited: 1,
    distanceTraveled: 0, acceptedMissions: 0, points: 0) ## The player's game's statistics

proc updateCraftingOrders*(index: string) =
  var updated = false
  for craftingOrder in gameStats.craftingOrders.mitems:
    if craftingOrder.index == index:
      craftingOrder.amount.inc
      updated = true
      break
  if not updated:
    gameStats.craftingOrders.add(y = StatisticsData(index: index, amount: 1))
  gameStats.points = gameStats.points + 5

# Temporary code for interfacing with Ada

proc updateAdaCraftingOrders(index: cstring) {.exportc.} =
  updateCraftingOrders(index = $index)
