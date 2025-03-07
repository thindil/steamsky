# Copyright 2025 Bartek thindil Jasicki
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

## Provides code for the player's reputation system with bases and factions,
## like updating reputation, storing it, etc.

import std/tables
import contracts
import config, factions, types, game

type
  ReputationObject = object
    ## Used to store data about the player's reputation in the selected faction
    factionIndex: string       ## Index of faction to which the reputation is related
    reputation: ReputationData ## The information about the reputation

var
  reputationsList: seq[ReputationObject] = @[]
    ## The list of player's reputation with all factions

proc resetReputations*() {.raises: [KeyError], tags: [],
    contractual.} =
  ## Reset the player's reputation with all factions
  reputationsList = @[]
  for index, faction in factionsList:
    let reputationLevel: int = if index == newGameSettings.playerFaction or
        isFriendly(sourceFaction = newGameSettings.playerFaction,
            targetFaction = index): 1 else: -1
    reputationsList.add(y = ReputationObject(factionIndex: index,
        reputation: ReputationData(level: reputationLevel, experience: 0)))

proc updateReputation*(baseIndex: BasesRange; amount: int) {.raises: [
    ReputationError], tags: [], contractual.} =
  ## Update the reputation in factions
  ##
  ## * baseIndex - the index of the base in which the reputation will be
  ##               updated
  ## * amount    - the amount of the reputation to be added or removed from
  ##               the player's reputations' levels
  let factionIndex: string = skyBases[baseIndex].owner
  var reputationIndex: int = -1
  for index, reputation in reputationsList:
    if reputation.factionIndex == factionIndex:
      reputationIndex = index
      break
  if reputationIndex == -1:
    raise newException(exceptn = ReputationError,
        message = "Can't find index of the faction")
  # Don't lose reputation below the lowest value
  if reputationsList[reputationIndex].reputation.level == -100 and amount < 0:
    return
  # Don't gain reputation above the highest value
  if reputationsList[reputationIndex].reputation.level == 100 and amount > 0:
    return
