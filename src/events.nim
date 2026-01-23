# Copyright 2023-2026 Bartek thindil Jasicki
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

## Provides code related to the in-game events like generating enemies,
## updating and deleting events, etc.

import std/[strutils, tables]
import contracts
import factions, game, maps, messages, reputation, types, utils

var playerShips: seq[Positive] = @[]

proc getPlayerShips*() {.raises: [], tags: [], contractual.} =
  ## Get the list of all prototype's ships which are available only for the
  ## player
  ensure:
    playerShips.len > 0
  body:
    playerShips = @[]
    for index, faction in factionsList:
      for career in faction.careers.values:
        playerShips.add(y = career.shipIndex)

proc generateEnemies*(enemies: var seq[Positive]; owner: string = "Any";
    withTraders: bool = true) {.raises: [KeyError], tags: [],
        contractual.} =
  ## Create a list of enemy's ships
  ##
  ## * enemies     - the list of enemy's ships which will be created
  ## * owner       - the faction to which the ships should belong. With default
  ##                 value, any enemy faction
  ## * withTraders - if true, add traders ships to the list, otherwise only
  ##                 combat ships. Default value is true
  ##
  ## Returns the updated paramater enemies
  require:
    owner.len > 0
  body:
    for index, ship in protoShipsList:
      var reputation: ReputationRange = getReputation(factionIndex = ship.owner)
      if getRandom(min = 1, max = 100) > 98:
        let newReputation = reputation * 2
        if newReputation > ReputationRange.high:
          reputation = ReputationRange.high
        elif newReputation < ReputationRange.low:
          reputation = ReputationRange.low
        else:
          reputation = newReputation
      if reputation <= -(ship.reputation) and (owner == "Any" or ship.owner ==
          owner) and not isFriendly(sourceFaction = playerShip.crew[0].faction,
          targetFaction = ship.owner) and index notin playerShips and (
          withTraders or tradersName notin ship.name):
        enemies.add(y = index)

proc deleteEvent*(eventIndex: Natural) {.raises: [],
    tags: [], contractual.} =
  ## Delete the selected event and update the map information
  ##
  ## * eventIndex - the index of the event to delete
  require:
    eventIndex < eventsList.len
  body:
    skyMap[eventsList[eventIndex].skyX][eventsList[
        eventIndex].skyY].eventIndex = -1
    eventsList.delete(i = eventIndex)
    for index, event in eventsList:
      skyMap[event.skyX][event.skyY].eventIndex = index

proc updateEvents*(minutes: Positive) {.raises: [], tags: [],
    contractual.} =
  ## Update timer for all known events, delete events if the timers passed
  ##
  ## * minutes - the amount of minutes passed in the game
  let eventsAmount: Natural = eventsList.len
  if eventsAmount == 0:
    return
  var key: Natural = 0
  while key < eventsList.len:
    let newTime: int = eventsList[key].time - minutes
    if newTime < 1:
      if eventsList[key].eType in {disease, attackOnBase} and getRandom(min = 1,
          max = 100) < 10:
        let baseIndex: ExtendedBasesRange = skyMap[eventsList[key].skyX][
            eventsList[key].skyY].baseIndex
        var populationLost: Positive = getRandom(min = 1, max = 10)
        if populationLost > skyBases[baseIndex].population:
          populationLost = skyBases[baseIndex].population
          skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)
        skyBases[baseIndex].population -= populationLost
      deleteEvent(eventIndex = key)
    else:
      eventsList[key].time = newTime
      key.inc
  if eventsAmount < eventsList.len:
    for index, event in eventsList:
      skyMap[event.skyX][event.skyY].eventIndex = index

proc recoverBase*(baseIndex: BasesRange) {.raises: [KeyError],
    tags: [], contractual.} =
  ## Set a new owner, population and reset dates for the abandoned base
  ##
  ## * baseIndex - the index of the base to recover
  var maxSpawnChance: Natural = 0
  for faction in factionsList.values:
    maxSpawnChance += faction.spawnChance
  var factionRoll: Natural = getRandom(min = 1, max = maxSpawnChance)
  for index, faction in factionsList:
    if factionRoll < faction.spawnChance:
      skyBases[baseIndex].owner = index
      skyBases[baseIndex].reputation.level = getReputation(
          sourceFaction = playerShip.crew[1].faction, targetFaction = skyBases[
          baseIndex].owner)
      break
    factionRoll -= faction.spawnChance
  skyBases[baseIndex].population = getRandom(min = 2, max = 50)
  skyBases[baseIndex].visited = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
  skyBases[baseIndex].recruitDate = DateRecord(year: 0, month: 0, day: 0,
      hour: 0, minutes: 0)
  skyBases[baseIndex].missionsDate = DateRecord(year: 0, month: 0, day: 0,
      hour: 0, minutes: 0)
  addMessage(message = "Base " & skyBases[baseIndex].name & " has a new owner.",
      mType = otherMessage, color = cyan)

proc generateFriendlyShips*(ships: var seq[Positive]) {.raises: [KeyError],
    tags: [], contractual.} =
  ## Create the list of ships which are frienly towards the player
  ##
  ## * ships - the list of friendly ships which will be created
  ##
  ## Returns the updated paramater ships
  for index, ship in protoShipsList:
    var reputation: int = getReputation(factionIndex = ship.owner)
    if getRandom(min = 1, max = 100) > 98:
      reputation *= 2
    if reputation >= ship.reputation and isFriendly(
        sourceFaction = playerShip.crew[0].faction,
        targetFaction = ship.owner) and index notin playerShips:
      ships.add(y = index)

proc generateTraders*(ships: var seq[Positive]) {.raises: [], tags: [],
    contractual.} =
  ## Create the list of traders' ships needed for events
  ##
  ## * ships - the list of traders which will be created
  ##
  ## Returns the updated paramater ships
  for index, ship in protoShipsList:
    var reputation: int = getReputation(factionIndex = ship.owner)
    if getRandom(min = 1, max = 100) > 98:
      reputation *= 2
    if reputation >= ship.reputation and ship.name.contains(sub = tradersName):
      ships.add(y = index)
