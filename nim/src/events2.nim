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
import combat, events, game, game2, items, maps, messages, shipscargo,
    shipscrew, shipscrew2, shipsmovement, types, utils

proc checkForEvent*(): bool =
  if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
    if eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType == enemyShip:
      return startCombat(enemyIndex = eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].shipIndex)
    else:
      return false
  if getRandom(min = 1, max = 100) > 6:
    return false
  var roll = getRandom(min = 1, max = 100)
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
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
    # Friendly ship
    of 24 .. 30:
      eventsList.add(EventData(eType: friendlyShip, skyX: playerShip.skyX,
          skyY: playerShip.skyY, time: getRandom(min = 30, max = 45),
          shipIndex: friendlyShips[getRandom(min = friendlyShips.low,
          max = friendlyShips.high)]))
      skyMap[playerShip.skyX][playerShip.skyY].eventIndex = eventsList.high
      addMessage(message = "You've spotted a friendly ship.",
          mType = otherMessage)
      gainPerception()
      updateOrders(ship = playerShip)
    # Combat
    else:
      var enemies: seq[Positive]
      generateEnemies(enemies = enemies)
      eventsList.add(EventData(eType: enemyShip, skyX: playerShip.skyX,
          skyY: playerShip.skyY, time: getRandom(min = 30, max = 45),
          shipIndex: enemies[getRandom(min = enemies.low, max = enemies.high)]))
      skyMap[playerShip.skyX][playerShip.skyY].eventIndex = eventsList.high
      return startCombat(enemyIndex = eventsList[eventsList.high].shipIndex)
  # Events at a sky base
  else:
    # Change owner of an abandoned base
    if skyBases[baseIndex].population == 0:
      if roll < 6 and playerShip.speed != docked:
        recoverBase(baseIndex = baseIndex)
      return false
    # Events inside a base
    if playerShip.speed == docked:
      # Brawl in base, happens only when there is more than 1 crew member
      if roll < 5 and playerShip.crew.len > 1:
        var restingCrew: seq[Positive]
        for index, member in playerShip.crew:
          if member.order == rest:
            restingCrew.add(index)
        if restingCrew.len > 0:
          let roll2 = getRandom(min = restingCrew.low, max = restingCrew.high)
          var injuries = getRandom(min = 1, max = 10)
          if injuries > playerShip.crew[restingCrew[roll2]].health:
            injuries = playerShip.crew[restingCrew[roll2]].health
          playerShip.crew[restingCrew[roll2]].health = playerShip.crew[
              restingCrew[roll2]].health - injuries
          addMessage(message = playerShip.crew[restingCrew[roll2]].name &
              " was injured in a brawl inside the base", mType = otherMessage, color = red)
          if playerShip.crew[restingCrew[roll2]].health == 0:
            death(memberIndex = restingCrew[roll2],
                reason = "injuries in brawl in base", ship = playerShip)
      # Lost cargo in the base
      elif roll > 4 and roll < 10:
        let roll2 = getRandom(min = playerShip.cargo.low,
            max = playerShip.cargo.high)
        var lostCargo = getRandom(min = 1, max = 10)
        if lostCargo > playerShip.cargo[roll2].amount:
          lostCargo = playerShip.cargo[roll2].amount
        addMessage(message = "During checking ship's cargo, you noticed that you lost " &
            $lostCargo & " " & getItemName(item = playerShip.cargo[roll2]) &
            ".", mType = otherMessage, color = red)
        updateCargo(ship = playerShip, amount = 0 - lostCargo,
            cargoIndex = roll2)
    # Events outside a base
    else:
      if roll in 21 .. 30 and skyBases[baseIndex].reputation.level == -100:
        roll = 31
      if "diseaseimmune" in factionsList[skyBases[baseIndex].owner].flags and
          roll == 21:
        roll = 20
      case roll
      # Base is attacked
      of 1 .. 20:
        var enemies: seq[Positive]
        generateEnemies(enemies = enemies, owner = "Any", withTraders = false)
        eventsList.add(EventData(eType: attackOnBase, skyX: playerShip.skyX,
            skyY: playerShip.skyY, time: getRandom(min = 60, max = 90),
            shipIndex: enemies[getRandom(min = enemies.low,
            max = enemies.high)]))
        addMessage(message = "You can't dock to base now, because base is under attack. You can help defend it.",
            mType = otherMessage)
        return startCombat(enemyIndex = eventsList[eventsList.high].shipIndex)
      # Disease in base
      of 21:
        eventsList.add(EventData(eType: disease, skyX: playerShip.skyX,
            skyY: playerShip.skyY, time: getRandom(min = 10_000, max = 12_000), data: 1))
        addMessage(message = "You can't dock to base now, it is closed due to disease.",
            mType = otherMessage)
      else:
        discard
