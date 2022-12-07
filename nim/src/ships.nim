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

  AdaMemberData = object
    attributes: array[17, array[2, cint]]
    skills: array[65, array[3, cint]]
    name: cstring
    gender: char
    health: cint
    tired: cint
    hunger: cint
    thirst: cint
    order: cint
    previousOrder: cint
    orderTime: cint
    orders: array[1..12, cint]
    equipment: array[0..6, cint]
    payment: array[1..2, cint]
    contractLength: cint
    morale: array[1..2, cint]
    loyalty: cint
    homeBase: cint
    faction: cstring

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
    of cabin:
      module = ModuleData(mType: cabin, cleanliness: adaModule.data[1],
          quality: adaModule.data[2])
    of turret:
      module = ModuleData(mType: turret, gunIndex: adaModule.data[1])
    of gun:
      module = ModuleData(mType: gun, damage: adaModule.data[1],
          ammoIndex: adaModule.data[2])
    of hull:
      module = ModuleData(mType: hull, installedModules: adaModule.data[1],
          maxModules: adaModule.data[2])
    of workshop:
      module = ModuleData(mType: workshop,
          craftingIndex: $adaModule.data[1], craftingTime: adaModule.data[2],
              craftingAmount: adaModule.data[3])
    of trainingRoom:
      module = ModuleData(mType: trainingRoom, trainedSkill: adaModule.data[1])
    of batteringRam:
      module = ModuleData(mType: batteringRam, damage2: adaModule.data[1],
          coolingDown: adaModule.data[2] == 1)
    of harpoonGun:
      module = ModuleData(mType: harpoonGun, duration: adaModule.data[1],
          harpoonIndex: adaModule.data[2])
    of ModuleType2.any:
      module = ModuleData(mType: ModuleType2.any, data: [1: adaModule.data[
          1].int, 2: adaModule.data[2].int, 3: adaModule.data[3].int])
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

proc getAdaShipCargo(cargo: array[1..128, AdaInventoryData]) {.exportc.} =
  playerShip.cargo = @[]
  for adaItem in cargo:
    if adaItem.protoIndex == 0:
      return
    playerShip.cargo.add(y = InventoryData(protoIndex: adaItem.protoIndex,
        amount: adaItem.amount, name: $adaItem.name,
        durability: adaItem.durability, price: adaItem.price))

proc getAdaShipCrew(crew: array[1..128, AdaMemberData]) {.exportc.} =
  playerShip.crew = @[]
  for adaMember in crew:
    if adaMember.name.len == 0:
      return
    var member = MemberData(name: $adaMember.name, gender: adaMember.gender,
        health: adaMember.health, tired: adaMember.tired,
        hunger: adaMember.hunger, thirst: adaMember.thirst,
        order: adaMember.order.CrewOrders,
        previousOrder: adaMember.previousOrder.CrewOrders,
        contractLength: adaMember.contractLength, loyalty: adaMember.loyalty,
        homeBase: adaMember.homeBase, faction: $adaMember.faction)
    for index, order in adaMember.orders.pairs:
      member.orders[index] = order
    for index, item in adaMember.equipment.pairs:
      member.equipment[index.EquipmentLocations] = item
    for attribute in adaMember.attributes:
      member.attributes.add(y = MobAttributeRecord(level: attribute[0],
          experience: attribute[1]))
    for skill in adaMember.skills:
      member.skills.add(y = SkillInfo(index: skill[0], level: skill[1],
          experience: skill[2]))
    member.payment = [adaMember.payment[1].Natural, adaMember.payment[2].Natural]
    member.morale = [adaMember.morale[1].Natural, adaMember.morale[2].Natural]
    playerShip.crew.add(y = member)

proc getAdaCrewInventory(inventory: array[1..128, AdaInventoryData];
    memberIndex: cint) {.exportc.} =
  playerShip.crew[memberIndex].inventory = @[]
  for adaItem in inventory:
    if adaItem.protoIndex == 0:
      return
    playerShip.crew[memberIndex].inventory.add(y = InventoryData(
        protoIndex: adaItem.protoIndex, amount: adaItem.amount,
        name: $adaItem.name,
        durability: adaItem.durability, price: adaItem.price))
