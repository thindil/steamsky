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

import std/[math, tables]
import bases, basestypes, factions, events, game, maps, messages, ships2,
    shipscrew, types, updategame, utils

proc askForEvents*() {.sideEffect, raises: [KeyError, Exception], tags: [
    WriteIOEffect, RootEffect].} =
  ## Ask for known events in a base or a friendly ship. Generates new
  ## events
  let traderIndex = findMember(order = talk)
  if traderIndex == -1:
    return
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var maxEvents: Natural
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
    let shipIndex = eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex
    maxEvents = (if protoShipsList[shipIndex].crew.len <
        5: 1 elif protoShipsList[shipIndex].crew.len < 10: 3 else: 5)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked ship '" & generateShipName(factionIndex = protoShipsList[
            shipIndex].owner) & "' for recent events.", mType = orderMessage)
    deleteEvent(eventIndex = skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex)
    updateOrders(ship = playerShip)
  let eventsAmount = getRandom(min = 1, max = maxEvents)
  var minX: cint = playerShip.skyX.cint - 100
  normalizeCoord(coord = minX)
  var maxX: cint = playerShip.skyX.cint + 100
  normalizeCoord(coord = maxX)
  var minY: cint = playerShip.skyY.cint - 100
  normalizeCoord(coord = minY, isXAxis = 0)
  var maxY: cint = playerShip.skyY.cint + 100
  normalizeCoord(coord = maxY, isXAxis = 0)
  var enemies: seq[Positive]
  generateEnemies(enemies)
  for i in 1 .. eventsAmount:
    var
      event = getRandom(min = 1, max = 5).EventsTypes
      attempts = 10
      eventX, eventY = 0
    while true:
      if event == enemyShip:
        eventX = getRandom(min = minX, max = maxX)
        eventY = getRandom(min = minY, max = maxY)
        if skyMap[eventX][eventY].baseIndex == 0 and eventX !=
            playerShip.skyX and eventY != playerShip.skyY and skyMap[eventX][
            eventY].eventIndex == -1:
          break
      else:
        let tmpBaseIndex = getRandom(min = 1, max = 1024)
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
        if eventX != playerShip.skyX and eventY != playerShip.skyY and skyMap[
            eventX][eventY].eventIndex == -1 and skyBases[skyMap[eventX][
            eventY].baseIndex].known:
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
      diffX = abs(playerShip.skyX - eventX)
      diffY = abs(playerShip.skyY - eventY)
      eventTime = (60 * sqrt((diffX ^ 2).float + (diffY ^ 2).float)).Natural
    case event
    of enemyShip:
      eventsList.add(EventData(eType: enemyShip, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime, max = eventTime + 60),
          shipIndex: enemies[getRandom(min = 0, max = enemies.len - 1)]))
    of attackOnBase:
      generateEnemies(enemies = enemies, owner = "Any", withTraders = false)
      eventsList.add(EventData(eType: attackOnBase, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime, max = eventTime + 120),
          shipIndex: enemies[getRandom(min = 0, max = enemies.len - 1)]))
      generateEnemies(enemies = enemies)
    of disease:
      eventsList.add(EventData(eType: disease, skyX: eventX,
          skyY: eventY, time: getRandom(min = 10_000, max = 12_000), data: 1))
    of doublePrice:
      var newItemIndex = 0
      block setDoublePrice:
        while true:
          var itemIndex = getRandom(min = 1, max = itemsList.len)
          for j in 1 .. itemsList.len:
            itemIndex.dec
            if itemIndex <= 0 and getPrice(baseType = skyBases[skyMap[eventX][
                eventY].baseIndex].baseType, itemIndex = j) > 0:
              newItemIndex = j
              break setDoublePrice
      eventsList.add(EventData(eType: doublePrice, skyX: eventX,
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

proc askForBases*() =
  let traderIndex = findMember(order = talk)
  if traderIndex == -1:
    return
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var amount, radius: Natural
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
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked for directions to other bases in base '" & skyBases[
            baseIndex].name &
        "'.", mType = orderMessage)
    gainRep(baseIndex = baseIndex, points = 1)
  else:
    radius = 40
    let shipIndex = eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex
    amount = (if protoShipsList[shipIndex].crew.len <
        5: 3 elif protoShipsList[shipIndex].crew.len < 10: 5 else: 10)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked ship '" & generateShipName(factionIndex = protoShipsList[
            shipIndex].owner) & "' for directions to other bases.",
            mType = orderMessage)
    deleteEvent(eventIndex = skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex)
    updateOrders(ship = playerShip)

# Temporary code for interfacing with Ada

proc askAdaForEvents() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc.} =
  try:
    askForEvents()
  except KeyError, IOError, Exception:
    discard

