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

import combat, events, game, game2, items, maps, messages, shipscargo,
    shipscrew, shipsmovement, types, utils

proc checkForEvent*(): bool =
  if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
    if eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType == enemyShip:
      return startCombat(enemyIndex = eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].shipIndex)
    else:
      return false
  if getRandom(min = 1, max = 100) > 6:
    return false
  let
    roll = getRandom(min = 1, max = 100)
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  # Event outside a sky base
  if baseIndex == 0:

    proc gainPerception() =
      for index, member in playerShip.crew:
        if member.order in {pilot, gunner}:
          gainExp(amount = 1, skillNumber = perceptionSkill, crewIndex = index)

    case roll
    # Engine damaged
    of 1 .. 5:
      let engineerIndex = findMember(order = engineer)
      if engineerIndex > -1 and playerShip.speed != fullStop:
        var roll2 = getRandom(min = 1, max = 100)
        case playerShip.speed
        of quarterSpeed:
          roll2 = if roll2 < 21: 1 else: roll2 - 20
        of fullSpeed:
          roll2 = roll2 + 20
        else:
          discard
        if roll2 > getSkillLevel(member = playerShip.crew[engineerIndex],
            skillIndex = engineeringSkill):
          addMessage(message = "One of your engines is taking damage.",
              mType = otherMessage, color = red)
          var engines: seq[Natural]
          for index, module in playerShip.modules:
            if module.mType == ModuleType2.engine and not module.disabled:
              engines.add(y = index)
          let engineIndex = engines[getRandom(min = 0, max = engines.high)]
          playerShip.modules[engineIndex].durability.dec
          updateOrders(ship = playerShip)
        else:
          addMessage(message = playerShip.crew[engineerIndex].name &
              " has prevented engine damage.", mType = otherMessage, color = green)
        gainExp(amount = 1, skillNumber = engineeringSkill,
            crewIndex = engineerIndex)
    # Bad weather
    of 6 .. 20:
      let pilotIndex = findMember(order = pilot)
      if pilotIndex > 0:
        addMessage(message = "Sudden bad weather causes your travel to take longer.",
            mType = otherMessage, color = red)
        var timePassed = 60 - getSkillLevel(member = playerShip.crew[
            pilotIndex], skillIndex = pilotingSkill)
        if timePassed < 1:
          timePassed = 1
        gainExp(amount = 1, skillNumber = pilotingSkill, crewIndex = pilotIndex)
        updateCargo(ship = playerShip, protoIndex = findProtoItem(
            itemType = fuelType), amount = countFuelNeeded())
        updateGame(minutes = timePassed)
    # Friendly trader
    of 21 .. 23:
      eventsList.add(EventData(eType: trader, skyX: playerShip.skyX,
          skyY: playerShip.skyY, time: getRandom(min = 30, max = 45),
          shipIndex: traders[getRandom(min = traders.low, max = traders.high)]))
      skyMap[playerShip.skyX][playerShip.skyY].eventIndex = eventsList.high
      addMessage(message = "You've meet a friendly trader.",
          mType = otherMessage)
      gainPerception()
      updateOrders(ship = playerShip)
    else:
      discard
