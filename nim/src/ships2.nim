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
import game, types, utils

proc countCombatValue*(): Natural {.sideEffect, raises: [KeyError], tags: [].} =
  ## Count the combat value of the player's ship based on its modules,
  ## weapons and ammunition.
  ##
  ## Returns the combat value of the player's ship.
  proc countAmmoValue(itemTypeIndex: Natural; multiple: Positive): Natural =
    for item in playerShip.cargo.items:
      if itemsList[item.protoIndex].itemType == itemsTypesList[itemTypeIndex]:
        result = result + itemsList[item.protoIndex].value[1] * multiple

  for module in playerShip.modules:
    case modulesList[module.protoIndex].mType
    of ModuleType.batteringRam:
      result = result + module.damage2
    of ModuleType.gun:
      result = result + module.maxDurability + (module.damage * 10)
      result = result + countAmmoValue(itemTypeIndex = modulesList[
          module.protoIndex].value, multiple = 10)
    of ModuleType.armor:
      result = result + module.maxDurability
    of ModuleType.harpoonGun:
      result = result + module.maxDurability + (module.duration * 5)
      result = result + countAmmoValue(itemTypeIndex = modulesList[
          module.protoIndex].value, multiple = 5)
    of ModuleType.hull:
      result = result + module.maxDurability + (module.maxModules * 10)
    else:
      discard

proc generateShipName*(factionIndex: string): string {.sideEffect, raises: [],
    tags: [].} =
  ## Generate the name for the ship, based on its owner's faction. Based
  ## on libtcod names generator
  ##
  ## * factionIndex - the index of the faction to which the ship belongs
  ##
  ## Returns the randomly generated name of the ship
  try:
    if factionsList[factionIndex].namesType == robotic:
      return $generateRoboticName()
  except KeyError:
    discard
  result = shipsSyllablesStartList[getRandom(min = 0, max = (
      shipsSyllablesStartList.len - 1))]
  if getRandom(min = 1, max = 100) < 51:
    result = result & shipsSyllablesMiddleList[getRandom(min = 0, max = (
        shipsSyllablesMiddleList.len - 1))]
  result = result & shipsSyllablesEndList[getRandom(min = 0, max = (
      shipsSyllablesEndList.len - 1))]

# Temporary code for interfacing with Ada

proc countAdaCombatValue(): cint {.raises: [], tags: [], exportc.} =
  try:
    return countCombatValue().cint
  except KeyError:
    return 0

proc generateAdaShipName(factionIndex: cstring): cstring {.sideEffect, raises: [
    ], tags: [], exportc.} =
  return generateShipName(factionIndex = $factionIndex).cstring


