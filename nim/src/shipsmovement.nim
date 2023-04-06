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

import game, crewinventory, messages, shipscargo, shipscrew2, types, utils

proc waitInPlace*(minutes: Positive) =
  if playerShip.speed == docked:
    return
  var baseFuelNeeded: int = 0
  for module in playerShip.modules:
    if module.mType == ModuleType2.engine and not module.disabled:
      baseFuelNeeded = baseFuelNeeded - 1
  var fuelNeeded = baseFuelNeeded * (minutes / 10).int
  if getRandom(min = 1, max = 10) < (minutes mod 10):
    fuelNeeded = fuelNeeded * baseFuelNeeded
  let fuelIndex = findItem(inventory = playerShip.cargo, itemType = fuelType)
  if fuelIndex == -1:
    addMessage(message = "Ship falls from the sky due to a lack of fuel.",
        mType = otherMessage, color = red)
    death(memberIndex = 0, reason = "fall of the ship", ship = playerShip)
    return
  if playerShip.cargo[fuelIndex].amount <= fuelNeeded.abs:
    addMessage(message = "Ship falls from the sky due to a lack of fuel.",
        mType = otherMessage, color = red)
    death(memberIndex = 0, reason = "fall of the ship", ship = playerShip)
    return
  updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
      fuelIndex].protoIndex, amount = fuelNeeded)

# Temporary code for interfacing with Ada

proc waitAdaInPlace(minutes: cint) {.raises: [], tags: [WriteIOEffect], exportc.} =
  try:
    waitInPlace(minutes = minutes.Positive)
  except KeyError, IOError:
    discard
