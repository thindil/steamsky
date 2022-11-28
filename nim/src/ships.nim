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

import std/tables
import game, types, utils

type
  ShipUpgrade = enum
    ## FUNCTION
    ##
    ## Available types of the player's ship's upgrades
    none, durability, maxValue, value

  ModuleType2 = enum
    ## FUNCTION
    ##
    ## Available types of the ships' modules
    workshop, any, medicalRoom, trainingRoom, engine, cabin, cockpit, turret,
        gun, cargoRoom, hull, armor, batteringRam, harpoonGun

  ModuleData = object
    ## FUNCTION
    ##
    ## Used to store information about ships' modules
    name: string ## The name of the module
    protoIndex: Natural ## The index of the prototype module
    weight: Natural ## The weight of the module
    durability: Natural ## The current durability of the module
    maxDurability: Natural ## The max durability of the module
    owner: seq[Natural] ## The list of owners of the module
    upgradeProgress: int ## The upgrade progess of the module
    upgradeAction: ShipUpgrade ## The current upgrade type for the module
    case mType: ModuleType2
    of engine:
      fuelUsage: Positive ## The fuel usage for engines modules
      power: Positive ## The power of the engines modules
      disabled: bool ## If true, the engine is disabled
    of cabin:
      cleanliness: Natural ## The cleanliness level of the cabin
      quality: Natural ## The quality level of the cabin
    of turret:
      gunIndex: Natural ## The index of the module used as gun in the turret
    of gun:
      damage: Positive ## The damage of the gun
      ammoIndex: Natural ## The index of item from ship's cargo used as ammunition
    of hull:
      installedModules: Natural ## The amount of installed modules in the hull
      maxModules: Positive ## The max amount of modules which the hull can hold
    of workshop:
      craftingIndex: string ## The index of currently crafted recipe
      craftingTime: Natural ## The amount of time needed to finish the order
      craftingAmount: Natural ## How many times repeat the crafting order
    of trainingRoom:
      trainedSkill: Natural ## The index of trained skill
    of batteringRam:
      damage2: Positive ## The damage of the battering ram
      coolingDown: bool ## If true, the battering ram can't attack now
    of harpoonGun:
      duration: Positive ## The duration bonus of the harpoon gun
      harpoonIndex: Natural ## The index of item from ship's cargo used as harpoon
    of any:
      data: array[1..3, int] ## Various data for module, depends on module
    else:
      discard

  ShipRecord* = object
    ## FUNCTION
    ##
    ## Used to store information about ships
    name: string ## The name of the ship
    skyX: MapXRange ## The X position of the ship on the map
    skyY: MapYRange ## The Y position of the ship on the map
    speed: ShipSpeed ## The current setting for the ship's speed
    modules: seq[ModuleData] ## The list of modules installed on the ship
    cargo: seq[InventoryData] ## The list of items in the ship's cargo
    crew: seq[MemberData] ## The list of the crew members of the ship
    upgradeModule: int ## The index of the currently upgraded module
    destinationX: range[0..MapXRange.high] ## The X position to which the ship goes
    destinationY: range[0..MapYRange.high] ## The Y position to which the ship goes
    repairModule: int ## The index of module which will be repaired as first
    description: string ## The description of the ship
    homeBase: Natural ## The index of the home base of the ship

var playerShip*: ShipRecord = ShipRecord(skyX: 1, skyY: 1) ## The player's ship's data

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

type
  AdaShipData = object
    name: cstring
    skyX: cint
    skyY: cint
    speed: cint
    upgradeModule: cint
    destinationX: cint
    destinationY: cint
    repairModule: cint
    description: cstring
    homeBase: cint

  AdaModuleData = object
    name: cstring
    protoIndex: cint
    weight: cint
    durability: cint
    maxDurability: cint
    owner: array[1..10, cint]
    upgradeProgress: cint
    upgradeAction: cint
    mType: cint
    data: array[1..3, cint]

proc generateAdaShipName(factionIndex: cstring): cstring {.sideEffect, raises: [
    ], tags: [], exportc.} =
  return generateShipName(factionIndex = $factionIndex).cstring

proc getAdaPlayerShip(shipData: AdaShipData) {.exportc.} =
  playerShip.name = $shipData.name
  playerShip.skyX = shipData.skyX
  playerShip.skyY = shipData.skyY
  playerShip.speed = shipData.speed.ShipSpeed
  playerShip.upgradeModule = shipData.upgradeModule
  playerShip.destinationX = shipData.destinationX
  playerShip.destinationY = shipData.destinationY
  playerShip.repairModule = shipData.repairModule
  playerShip.description = $shipData.description
  playerShip.homeBase = shipData.homeBase

proc getAdaShipModules(modules: array[1..75, AdaModuleData]) {.exportc.} =
  playerShip.modules = @[]
  for adaModule in modules:
    if adaModule.mType == ModuleType2.any.ord:
      return
    var module: ModuleData
    case adaModule.mType.ModuleType2
    of engine:
      module = ModuleData(mType: engine,
          fuelUsage: adaModule.data[1], power: adaModule.data[2],
          disabled: adaModule.data[3] == 1)
    else:
      discard
    module.name = $adaModule.name
    module.protoIndex = adaModule.protoIndex
    module.weight = adaModule.weight
    module.durability = adaModule.durability
    module.maxDurability = adaModule.maxDurability
    module.upgradeProgress = adaModule.upgradeProgress
    module.upgradeAction = adaModule.upgradeAction.ShipUpgrade
    for owner in adaModule.owner:
      if owner == 0:
        break
      module.owner.add(y = owner)
    playerShip.modules.add(y = module)
