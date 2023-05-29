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

import std/[strutils, tables]
import factions, game, maps, messages, ships2, types, utils

proc getPlayerShips(playerShips: var seq[Positive]) {.sideEffect, raises: [],
    tags: [].} =
  ## Get the list of all prototype's ships which are available only for the
  ## player
  ##
  ## * playerShips - the list of ships' prototypes available for the player
  ##
  ## Returns the updated parameter playerShips.
  for index, faction in factionsList.pairs:
    for career in faction.careers.values:
      playerShips.add(y = career.shipIndex)

proc generateEnemies*(enemies: var seq[Positive]; owner: string = "Any";
    withTraders: bool = true) {.sideEffect, raises: [KeyError], tags: [].} =
  ## Create a list of enemy's ships
  ##
  ## * enemies     - the list of enemy's ships which will be created
  ## * owner       - the faction to which the ships should belong. With default
  ##                 value, any enemy faction
  ## * withTraders - if true, add traders ships to the list, otherwise only
  ##                 combat ships. Default value is true
  ##
  ## Returns the updated paramater enemies
  var playerValue = countCombatValue()
  if getRandom(min = 1, max = 100) > 98:
    playerValue = playerValue * 2
  var playerShips: seq[Positive]
  getPlayerShips(playerShips = playerShips)
  for index, ship in protoShipsList:
    if ship.combatValue <= playerValue and (owner == "Any" or ship.owner ==
        owner) and not isFriendly(sourceFaction = playerShip.crew[0].faction,
        targetFaction = ship.owner) and index notin playerShips and (
        withTraders or tradersName notin ship.name):
      enemies.add(y = index)

proc updateEvents*(minutes: Positive) {.sideEffect, raises: [KeyError], tags: [].} =
  ## Update timer for all known events, delete events if the timers passed
  ##
  ## * minutes - the amount of minutes passed in the game
  let eventsAmount = eventsList.len
  if eventsAmount == 0:
    return
  for key in 1 .. eventsAmount:
    if key notin eventsList:
      continue
    let newTime = eventsList[key].time - minutes
    if newTime < 1:
      if eventsList[key].eType in {disease, attackOnBase} and getRandom(min = 1,
          max = 100) < 10:
        let baseIndex = skyMap[eventsList[key].skyX][eventsList[
            key].skyY].baseIndex
        var populationLost = getRandom(min = 1, max = 10)
        if populationLost > skyBases[baseIndex].population:
          populationLost = skyBases[baseIndex].population
          skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)
        skyBases[baseIndex].population = skyBases[baseIndex].population - populationLost
      skyMap[eventsList[key].skyX][eventsList[key].skyY].eventIndex = 0
      {.warning[UnsafeDefault]: off.}
      eventsList.del(key)
      {.warning[UnsafeDefault]: on.}
    else:
      eventsList[key].time = newTime
  if eventsAmount < eventsList.len:
    for key in eventsList.keys:
      skyMap[eventsList[key].skyX][eventsList[key].skyY].eventIndex = key

proc deleteEvent*(eventIndex: Positive) {.sideEffect, raises: [KeyError],
    tags: [].} =
  ## Delete the selected event and update the map information
  ##
  ## * eventIndex - the index of the event to delete
  skyMap[eventsList[eventIndex].skyX][eventsList[
      eventIndex].skyY].eventIndex = 0
  eventsList.del(key = eventIndex)
  for index, event in eventsList.pairs:
    skyMap[event.skyX][event.skyY].eventIndex = index

proc recoverBase*(baseIndex: BasesRange) {.sideEffect, raises: [KeyError],
    tags: [].} =
  ## Set a new owner, population and reset dates for the abandoned base
  ##
  ## * baseIndex - the index of the base to recover
  var maxSpawnChance: Natural = 0
  for faction in factionsList.values:
    maxSpawnChance = maxSpawnChance + faction.spawnChance
  var factionRoll = getRandom(min = 1, max = maxSpawnChance)
  for index, faction in factionsList.pairs:
    if factionRoll < faction.spawnChance:
      skyBases[baseIndex].owner = index
      skyBases[baseIndex].reputation.level = getReputation(
          sourceFaction = playerShip.crew[1].faction, targetFaction = skyBases[
          baseIndex].owner)
      break
    factionRoll = factionRoll - faction.spawnChance
  skyBases[baseIndex].population = getRandom(min = 2, max = 50)
  skyBases[baseIndex].visited = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
  skyBases[baseIndex].recruitDate = DateRecord(year: 0, month: 0, day: 0,
      hour: 0, minutes: 0)
  skyBases[baseIndex].missionsDate = DateRecord(year: 0, month: 0, day: 0,
      hour: 0, minutes: 0)
  addMessage(message = "Base " & skyBases[baseIndex].name & " has a new owner.",
      mType = otherMessage, color = cyan)

# Temporary code for interfacing with Ada

proc getAdaEvent(index, x, y, time, eType, data: cint) {.raises: [], tags: [], exportc.} =
  var event = EventData(skyX: x, skyY: y, time: time, eType: eType.EventsTypes)
  case event.eType
    of doublePrice:
      event.itemIndex = data
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      event.shipIndex = data
    else:
      event.data = data
  eventsList[index] = event

proc setAdaEvent(index: cint; x, y, time, eType, data: var cint) {.raises: [],
    tags: [], exportc.} =
  try:
    x = eventsList[index].skyX
    y = eventsList[index].skyY
    time = eventsList[index].time.cint
    eType = eventsList[index].eType.cint
    case eventsList[index].eType
    of doublePrice:
      data = eventsList[index].itemIndex.cint
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      data = eventsList[index].shipIndex.cint
    else:
      data = eventsList[index].data.cint
  except KeyError:
    x = -1

proc getAdaPlayerShips(playerShips: var array[30, cint]) {.raises: [], tags: [], exportc.} =
  for ship in playerShips.mitems:
    ship = 0
  var nimShips: seq[Positive]
  getPlayerShips(playerShips = nimShips)
  for index, ship in nimShips.pairs:
    playerShips[index] = ship.cint

proc generateAdaEnemies(enemies: var array[300, cint]; owner: cstring;
    withTraders: cint) {.raises: [], tags: [], exportc.} =
  for ship in enemies.mitems:
    ship = 0
  var nimShips: seq[Positive]
  try:
    generateEnemies(enemies = nimShips, owner = $owner,
        withTraders = withTraders == 1)
  except KeyError:
    return
  for index, ship in nimShips.pairs:
    enemies[index] = ship.cint

proc updateAdaEvents(minutes: cint) {.raises: [], tags: [], exportc.} =
  try:
    updateEvents(minutes = minutes)
  except KeyError:
    discard

proc clearAdaEvents() {.raises: [], tags: [], exportc.} =
  eventsList.clear()

proc deleteAdaEvent(eventIndex: cint) {.raises: [], tags: [], exportc.} =
  try:
    deleteEvent(eventIndex = eventIndex)
  except KeyError:
    discard

proc recoverAdaBase(baseIndex: cint) {.raises: [], tags: [], exportc.} =
  try:
    recoverBase(baseIndex = baseIndex)
  except KeyError:
    discard
