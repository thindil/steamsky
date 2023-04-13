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

proc repairShip*(minutes: Positive) =

  var
    crewRepairPoints: seq[Natural]
    repairPoints: int = 0
    repairStopped = false

  proc repairModule(moduleIndex: Natural) =
    var
      pointsIndex = -1
      pointsBonus: int
    repairStopped = false
    for index, member in playerShip.crew.pairs:
      if member.order == repair:
        pointsIndex.inc
        if crewRepairPoints[pointsIndex] > 0:
          pointsBonus = (getSkillLevel(member = member,
              skillIndex = modulesList[playerShip.modules[
              moduleIndex].protoIndex].repairSkill) / 10).int *
              crewRepairPoints[pointsIndex]
          repairPoints = crewRepairPoints[pointsIndex] + pointsBonus
          let toolsIndex = findTools(memberIndex = index,
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
