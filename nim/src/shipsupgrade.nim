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

import std/tables
import crewinventory, game, items, messages, shipscrew, types

proc upgradeShip*(minutes: Positive) =

  var upgradeMaterial, upgradeTools, workerIndex = -1
  var upgradedModule: ModuleData

  proc findMatsAndTools() =
    upgradeTools = findTools(memberIndex = workerIndex, itemType = repairTools,
        order = upgrading)
    upgradeMaterial = findItem(inventory = playerShip.cargo,
        itemType = modulesList[upgradedModule.protoIndex].repairMaterial)

  proc maxUpgradeReached(messageText: string) =
    addMessage(message = messageText & upgradedModule.name & ".",
        mtype = orderMessage, color = yellow)
    upgradedModule.upgradeProgress = 0
    upgradedModule.upgradeAction = none
    playerShip.modules[playerShip.upgradeModule] = upgradedModule
    playerShip.upgradeModule = -1
    giveOrders(ship = playerShip, memberIndex = workerIndex, givenOrder = rest)

  if playerShip.upgradeModule == -1:
    return
  workerIndex = findMember(order = upgrading)
  if workerIndex == -1:
    return
  upgradedModule = playerShip.modules[playerShip.upgradeModule]
  var
    currentMinutes = minutes
    orderTime = playerShip.crew[workerIndex].orderTime
  if upgradedModule.durability == 0:
    addMessage(message = playerShip.crew[workerIndex].name &
        " stops upgrading " & upgradedModule.name & " because it's destroyed.",
        mType = orderMessage, color = red)
    giveOrders(ship = playerShip, memberIndex = workerIndex, givenOrder = rest)
  var times = 0
  while currentMinutes > 0:
    if currentMinutes >= orderTime:
      currentMinutes = currentMinutes - orderTime
      times.inc
      orderTime = 15
    else:
      orderTime = orderTime - currentMinutes
      currentMinutes = 0
  playerShip.crew[workerIndex].orderTime = orderTime
  if times == 0:
    return
  var upgradePoints = ((getSkillLevel(member = playerShip.crew[workerIndex],
      skillIndex = modulesList[upgradedModule.protoIndex].repairSkill) /
      10).int * times) + times
  while upgradePoints > 0 and upgradedModule.upgradeProgress > 0:
    var resultAmount = upgradePoints
    if resultAmount > upgradedModule.upgradeProgress:
      resultAmount = upgradedModule.upgradeProgress
    findMatsAndTools()
    if upgradeMaterial == -1:
      addMessage(message = "You don't have enough materials to upgrade " &
          upgradedModule.name, mtype = orderMessage, color = red)
      giveOrders(ship = playerShip, memberIndex = workerIndex,
          givenOrder = rest)
      break
    if upgradeTools == -1:
      addMessage(message = "You don't have the repair tool to upgrade " &
          upgradedModule.name, mtype = orderMessage, color = red)
      giveOrders(ship = playerShip, memberIndex = workerIndex,
          givenOrder = rest)
      break
