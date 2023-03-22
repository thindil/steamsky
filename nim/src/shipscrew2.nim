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

import game, missions, types

proc deleteMember*(memberIndex: Natural; ship: var ShipRecord) {.sideEffect,
    raises: [KeyError], tags: [].} =
  ## Delete the selected member from the selected ship crew list, update
  ## the ship modules with the new crew list and delete accepted missions
  ## if neccessary.
  ##
  ## * memberIndex - the crew index of the member to delete
  ## * ship        - the ship from which the crew member will be deleted
  ##
  ## Returns parameter ship with the updated list of crew members and modules
  var deleted = false
  if ship.crew == playerShip.crew:
    for index, mission in acceptedMissions.pairs:
      if mission.mType == passenger and mission.data == memberIndex:
        deleteMission(missionIndex = index)
        deleted = true
        break
    for mission in acceptedMissions.mitems:
      if mission.mType == passenger and mission.data > memberIndex:
        mission.data.inc
    if deleted:
      return
  ship.crew.delete(i = memberIndex)
  for module in ship.modules.mitems:
    for owner in module.owner.mitems:
      if owner == memberIndex:
        owner = 0
      elif owner > memberIndex:
        owner.dec

# Temporary code for interfacing with Ada

proc deleteAdaMember(memberIndex, inPlayerShip: cint) {.raises: [], tags: [], exportc.} =
  try:
    if inPlayerShip == 1:
      deleteMember(memberIndex = memberIndex - 1, ship = playerShip)
    else:
      deleteMember(memberIndex = memberIndex - 1, ship = npcShip)
  except KeyError:
    discard
