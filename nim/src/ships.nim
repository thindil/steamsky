# Copyright 2022 Bartek thindil Jasicki
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

{.used.}

import std/tables
import factions, game, items, utils

type
  ShipUpgrade = enum
    none, durability, maxValue, value

  ModuleType2 = enum
    workshop, any, medicalRoom, trainingRoom, engine, cabin, cockpit, turret,
        gun, cargoRoom, hull, armor, batteringRam, harpoonGun

  ModuleData = object
    name: string
    protoIndex: Natural
    weight: Natural
    durability: Natural
    maxDurability: Natural
    owner: seq[Natural]
    upgradeProgress: int
    upgradeAction: ShipUpgrade
    case mType: ModuleType2
    of engine:
      fuelUsage: Positive
      power: Positive
      disabled: bool
    of cabin:
      cleanliness: Natural
      quality: Natural
    of turret:
      gunIndex: Natural
    of gun:
      damage: Positive
      ammoIndex: Natural
    of hull:
      installedModules: Natural
      maxModules: Positive
    of workshop:
      craftingIndex: string
      craftingTime: Natural
      craftingAmount: Natural
    of trainingRoom:
      trainedSkill: Natural
    of batteringRam:
      damage2: Positive
      coolingDown: bool
    of harpoonGun:
      duration: Positive
      harpoonIndex: Natural
    of any:
      data: array[1..3, int]
    else:
      discard

  ShipRecord = object
    modules: Table[Positive, ModuleData]
    cargo: Table[Positive, InventoryData]

func getCabinQuality*(quality: cint): cstring {.gcsafe, raises: [], tags: [], exportc.} =
  ## FUNCTION
  ##
  ## Get the description of quality of the selected cabin in the player's ship
  ##
  ## PARAMETERS
  ##
  ## * quality - The numerical value of the cabin's quality which will be
  ##             converted to string
  ##
  ## RETURNS
  ##
  ## The string with the description of the cabin's quality
  case quality
  of 0..10:
    return "Empty room"
  of 11..20:
    return "Minimal quality"
  of 21..30:
    return "Basic quality"
  of 31..40:
    return "Second class"
  of 41..50:
    return "Medium quality"
  of 51..60:
    return "First class"
  of 61..70:
    return "Extended quality"
  of 71..80:
    return "Encrusted room"
  of 81..90:
    return "Luxury quality"
  else:
    return "Palace room"

proc generateShipName*(factionIndex: string): string {.sideEffect, raises: [],
    tags: [].} =
  ## FUNCTION
  ##
  ## Generate the name for the ship, based on its owner's faction. Based
  ## on libtcod names generator
  ##
  ## PARAMETERS
  ##
  ## * factionIndex - the index of the faction to which the ship belongs
  ##
  ## RETURNS
  ##
  ## The randomly generated name of the ship
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

proc generateAdaShipName(factionIndex: cstring): cstring {.sideEffect, raises: [
    ], tags: [], exportc.} =
  return generateShipName(factionIndex = $factionIndex).cstring
