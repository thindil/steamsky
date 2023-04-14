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
import crewinventory, game, items, messages, shipscargo, shipscrew, types

proc repairShip*(minutes: Positive) =

  var
    crewRepairPoints: seq[Natural]
    repairPoints: int = 0
    repairStopped = false
    repairNeeded = true

  proc repairModule(moduleIndex: Natural) =
    var
      pointsIndex = -1
      pointsBonus: int
    repairStopped = false
    for index, member in playerShip.crew.mpairs:
      if member.order == repair:
        pointsIndex.inc
        if crewRepairPoints[pointsIndex] > 0:
          pointsBonus = (getSkillLevel(member = member,
              skillIndex = modulesList[playerShip.modules[
              moduleIndex].protoIndex].repairSkill) / 10).int *
              crewRepairPoints[pointsIndex]
          repairPoints = crewRepairPoints[pointsIndex] + pointsBonus
          var toolsIndex = findTools(memberIndex = index,
              itemType = repairTools, order = repair)
          if toolsIndex == -1:
            if pointsIndex == 0:
              addMessage(message = "You don't have the proper repair tools to continue repairs of " &
                  playerShip.modules[moduleIndex].name & ".",
                  mType = orderMessage, color = red)
            else:
              addMessage(message = member.name &
                  " can't continue repairs due to a lack of repair tools.",
                  mType = orderMessage, color = red)
            repairStopped = true
            return
          var repairMaterial = findItem(inventory = playerShip.cargo,
              itemType = modulesList[playerShip.modules[
              moduleIndex].protoIndex].repairMaterial)
          if repairMaterial > -1 and playerShip.cargo[repairMaterial].amount < repairPoints:
            repairPoints = playerShip.cargo[repairMaterial].amount
          if repairMaterial == -1:
            addMessage(message = "You don't have the proper repair materials to continue repairs of " &
                playerShip.modules[moduleIndex].name & ".",
                mType = orderMessage, color = red)
            repairStopped = true
            return
          var repairValue = 0
          if playerShip.modules[moduleIndex].durability + repairPoints >=
              playerShip.modules[moduleIndex].maxDurability:
            repairValue = playerShip.modules[moduleIndex].maxDurability -
                playerShip.modules[moduleIndex].durability
            repairNeeded = false
          else:
            repairValue = repairPoints
          if repairValue == playerShip.cargo[repairMaterial].amount and
              toolsIndex > repairMaterial:
            toolsIndex.dec
          updateCargo(ship = playerShip, cargoIndex = repairMaterial,
              amount = -(repairValue))
          playerShip.modules[moduleIndex].durability = playerShip.modules[
              moduleIndex].durability + repairValue
          if repairValue > crewRepairPoints[pointsIndex]:
            repairValue = crewRepairPoints[pointsIndex]
            repairPoints = 0
          else:
            repairPoints = crewRepairPoints[pointsIndex] - repairValue
          gainExp(amount = repairValue, skillNumber = modulesList[
              playerShip.modules[moduleIndex].protoIndex].repairSkill,
              crewIndex = index)
          crewRepairPoints[pointsIndex] = repairPoints
          damageItem(inventory = member.inventory, itemIndex = toolsIndex,
              skillLevel = getSkillLevel(member = member,
              skillIndex = modulesList[playerShip.modules[
              moduleIndex].protoIndex].repairSkill), memberIndex = index,
              ship = playerShip)
          if not repairNeeded:
            break

  var currentMinutes, orderTime = 0
  for member in playerShip.crew.mitems:
    if member.order == repair:
      currentMinutes = minutes
      orderTime = member.orderTime
      repairPoints = 0
      while currentMinutes > 0:
        if currentMinutes >= orderTime:
          currentMinutes = currentMinutes - orderTime
          repairPoints.inc
          orderTime = 15
        else:
          orderTime = orderTime - currentMinutes
          currentMinutes = 0
      crewRepairPoints.add(y = repairPoints)
      member.orderTime = orderTime
  if crewRepairPoints.len == 0:
    return
  if playerShip.repairModule > -1 and playerShip.modules[
      playerShip.repairModule].durability < playerShip.modules[
      playerShip.repairModule].maxDurability:
    repairModule(moduleIndex = playerShip.repairModule)
  for index, module in playerShip.modules.pairs:
    if module.durability < module.maxDurability:
      repairModule(moduleIndex = index)
  if not repairNeeded or repairStopped:
    if not repairNeeded:
      addMessage(message = "All repairs have been finished.",
          mType = orderMessage, color = green)
    for index, member in playerShip.crew.mpairs:
      if member.order == repair:
        giveOrders(ship = playerShip, memberIndex = index, givenOrder = rest)

# Temporary code for interfacing with Ada

proc repairAdaShip(minutes: cint) {.raises: [], tags: [RootEffect], exportc.} =
  try:
    repairShip(minutes = minutes)
  except KeyError, Exception:
    discard
