# Copyright 2023-2024 Bartek thindil Jasicki
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

## Provides code related to the sky bases interactions like asking for events or
## bases. Split from the bases module to avoid circular dependencies.

import std/[math, tables]
import contracts
import bases, basestypes, factions, events, game, game2, maps, messages, ships2,
    shipscrew, types, utils

proc askForEvents*() {.sideEffect, raises: [KeyError, Exception], tags: [
    WriteIOEffect, RootEffect], contractual.} =
  ## Ask for known events in a base or a friendly ship. Generates new
  ## events
  let traderIndex: int = findMember(order = talk)
  if traderIndex == -1:
    return
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var maxEvents: Natural = 0
  # Asking in base
  if baseIndex > 0:
    maxEvents = (if skyBases[baseIndex].population < 150: 5 elif skyBases[
        baseIndex].population < 300: 10 else: 15)
    skyBases[baseIndex].askedForEvents = gameDate
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked for recent events known at base '" & skyBases[baseIndex].name &
        "'.", mType = orderMessage)
    gainRep(baseIndex = baseIndex, points = 1)
  else:
    let shipIndex: Positive = eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex
    maxEvents = (if protoShipsList[shipIndex].crew.len <
        5: 1 elif protoShipsList[shipIndex].crew.len < 10: 3 else: 5)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked ship '" & generateShipName(factionIndex = protoShipsList[
            shipIndex].owner) & "' for recent events.", mType = orderMessage)
    deleteEvent(eventIndex = skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex)
    updateOrders(ship = playerShip)
  let eventsAmount: Positive = getRandom(min = 1, max = maxEvents)
  var minX: int = playerShip.skyX - 100
  normalizeCoord(coord = minX)
  var maxX: int = playerShip.skyX + 100
  normalizeCoord(coord = maxX)
  var minY: int = playerShip.skyY - 100
  normalizeCoord(coord = minY, isXAxis = false)
  var maxY: int = playerShip.skyY + 100
  normalizeCoord(coord = maxY, isXAxis = false)
  var enemies: seq[Positive] = @[]
  generateEnemies(enemies = enemies)
  for i in 1..eventsAmount:
    var
      event: EventsTypes = getRandom(min = 1, max = 5).EventsTypes
      attempts: int = 10
      eventX, eventY: int = 0
    while true:
      if event == enemyShip:
        eventX = getRandom(min = minX, max = maxX)
        eventY = getRandom(min = minY, max = maxY)
        if skyMap[eventX][eventY].baseIndex == 0 and eventX !=
            playerShip.skyX and eventY != playerShip.skyY and skyMap[eventX][
            eventY].eventIndex == -1:
          break
      else:
        let tmpBaseIndex: Positive = getRandom(min = 1, max = 1024)
        eventX = skyBases[tmpBaseIndex].skyX
        eventY = skyBases[tmpBaseIndex].skyY
        attempts.dec
        if attempts == 0:
          event = enemyShip
          while true:
            eventX = getRandom(min = minX, max = maxX)
            eventY = getRandom(min = minY, max = maxY)
            if skyMap[eventX][eventY].baseIndex == 0 and eventX !=
                playerShip.skyX and eventY != playerShip.skyY and skyMap[
                    eventX][
                eventY].eventIndex == -1:
              break
          break
        if eventX != playerShip.skyX and eventY != playerShip.skyY and
          skyMap[eventX][eventY].eventIndex == -1 and
          skyBases[skyMap[eventX][eventY].baseIndex].known:
          if event == attackOnBase and skyBases[skyMap[eventX][
              eventY].baseIndex].population > 0:
            break
          if event == doublePrice and isFriendly(
              sourceFaction = playerShip.crew[0].faction,
              targetFaction = skyBases[skyMap[eventX][eventY].baseIndex].owner):
            break
          if event == disease and "diseaseimmnune" in factionsList[skyBases[
              skyMap[eventX][eventY].baseIndex].owner].flags and isFriendly(
              sourceFaction = playerShip.crew[0].faction,
              targetFaction = skyBases[skyMap[eventX][eventY].baseIndex].owner):
            break
          if event == baseRecovery and skyBases[skyMap[eventX][
              eventY].baseIndex].population == 0:
            break
    let
      diffX: Natural = abs(x = playerShip.skyX - eventX)
      diffY: Natural = abs(x = playerShip.skyY - eventY)
      eventTime: Natural = (60 * sqrt(x = (diffX ^ 2).float + (diffY ^ 2).float)).Natural
    case event
    of enemyShip:
      eventsList.add(y = EventData(eType: enemyShip, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime, max = eventTime + 60),
          shipIndex: enemies[getRandom(min = 0, max = enemies.len - 1)]))
    of attackOnBase:
      generateEnemies(enemies = enemies, owner = "Any", withTraders = false)
      eventsList.add(y = EventData(eType: attackOnBase, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime, max = eventTime + 120),
          shipIndex: enemies[getRandom(min = 0, max = enemies.len - 1)]))
      generateEnemies(enemies = enemies)
    of disease:
      eventsList.add(y = EventData(eType: disease, skyX: eventX,
          skyY: eventY, time: getRandom(min = 10_000, max = 12_000), data: 1))
    of doublePrice:
      var newItemIndex: int = 0
      block setDoublePrice:
        while true:
          var itemIndex: Positive = getRandom(min = 1, max = itemsList.len)
          for j in 1 .. itemsList.len:
            itemIndex.dec
            if itemIndex <= 0 and getPrice(baseType = skyBases[skyMap[eventX][
                eventY].baseIndex].baseType, itemIndex = j) > 0:
              newItemIndex = j
              break setDoublePrice
      eventsList.add(y = EventData(eType: doublePrice, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime * 3, max = eventTime *
              4), itemIndex: newItemIndex))
    of baseRecovery:
      recoverBase(baseIndex = skyMap[eventX][eventY].baseIndex)
    else:
      discard
    if event != baseRecovery:
      skyMap[eventX][eventY].eventIndex = eventsList.len - 1
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  updateGame(minutes = 30)

proc askForBases*() {.sideEffect, raises: [KeyError, Exception], tags: [
    WriteIOEffect, RootEffect], contractual.} =
  ## Ask for known bases in a base or a friendly ship.
  let traderIndex: int = findMember(order = talk)
  if traderIndex == -1:
    return
  let
    baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    shipIndex: Natural = (if baseIndex == 0: eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex else: 0)
  var amount, radius: Natural = 0
  # Asking in base
  if baseIndex > 0:
    if skyBases[baseIndex].population < 150:
      amount = 10
      radius = 10
    elif skyBases[baseIndex].population < 300:
      amount = 20
      radius = 20
    else:
      amount = 40
      radius = 40
    skyBases[baseIndex].askedForBases = true
    gainRep(baseIndex = baseIndex, points = 1)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked for directions to other bases in base '" & skyBases[
            baseIndex].name &
        "'.", mType = orderMessage)
  # Asking friendly ship
  else:
    radius = 40
    amount = (if protoShipsList[shipIndex].crew.len <
        5: 3 elif protoShipsList[shipIndex].crew.len < 10: 5 else: 10)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked ship '" & generateShipName(factionIndex = protoShipsList[
            shipIndex].owner) & "' for directions to other bases.",
            mType = orderMessage)
    deleteEvent(eventIndex = skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex)
    updateOrders(ship = playerShip)
  block findBases:
    for x in -radius..radius:
      for y in -radius..radius:
        var
          tempX: int = playerShip.skyX + x
          tempY: int = playerShip.skyY + y
        normalizeCoord(coord = tempX)
        normalizeCoord(coord = tempY, isXAxis = false)
        let tmpBaseIndex: Natural = skyMap[tempX][tempY].baseIndex
        if tmpBaseIndex > 0 and not skyBases[tmpBaseIndex].known:
          skyBases[tmpBaseIndex].known = true
          amount.dec
          if amount == 0:
            break findBases
  if amount > 0:
    if baseIndex > 0:
      if skyBases[baseIndex].population < 150 and amount > 1:
        amount = 1
      elif skyBases[baseIndex].population < 300 and amount > 2:
        amount = 2
      elif amount > 4:
        amount = 4
    else:
      amount = (if protoShipsList[shipIndex].crew.len <
          5: 1 elif protoShipsList[shipIndex].crew.len < 10: 2 else: 4)
    var unknownBases: int = 0
    for base in skyBases:
      if not base.known:
        unknownBases.inc
      if unknownBases >= amount:
        break
    if unknownBases >= amount:
      while true:
        let tmpBaseIndex: Positive = getRandom(min = 1, max = 1_024)
        if not skyBases[tmpBaseIndex].known:
          skyBases[tmpBaseIndex].known = true
          amount.dec
        if amount == 0:
          break
    else:
      for base in skyBases.mitems:
        if not base.known:
          base.known = true
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  updateGame(minutes = 30)

# Temporary code for interfacing with Ada

proc askAdaForEvents() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc,
    contractual.} =
  ## Temporary C binding
  try:
    askForEvents()
  except KeyError, IOError, Exception:
    discard

proc askAdaForBases() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc,
    contractual.} =
  ## Temporary C binding
  try:
    askForBases()
  except KeyError, IOError, Exception:
    discard

