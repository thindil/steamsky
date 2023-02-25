# Copyright 2022-2023 Bartek thindil Jasicki
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

var
  playerShip*: ShipRecord = ShipRecord(skyX: 1, skyY: 1) ## The player's ship's data
  npcShip*: ShipRecord = ShipRecord(skyX: 1, skyY: 1) ## The npc ship like enemy, trader, etc

func getCabinQuality*(quality: cint): cstring {.gcsafe, raises: [], tags: [], exportc.} =
  ## Get the description of quality of the selected cabin in the player's ship
  ##
  ## * quality - The numerical value of the cabin's quality which will be
  ##             converted to string
  ##
  ## Returns the string with the description of the cabin's quality
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
    data2: cstring

  AdaMemberData* = object
    attributes: array[1..16, array[2, cint]]
    skills: array[1..64, array[3, cint]]
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

proc getAdaShip(shipData: AdaShipData; getPlayerShip: cint = 1) {.raises: [],
    tags: [], exportc.} =
  if getPlayerShip == 1:
    playerShip.name = $shipData.name
    playerShip.skyX = shipData.skyX
    playerShip.skyY = shipData.skyY
    playerShip.speed = shipData.speed.ShipSpeed
    playerShip.upgradeModule = shipData.upgradeModule - 1
    playerShip.destinationX = shipData.destinationX
    playerShip.destinationY = shipData.destinationY
    playerShip.repairModule = shipData.repairModule - 1
    playerShip.description = $shipData.description
    playerShip.homeBase = shipData.homeBase
  else:
    npcShip.name = $shipData.name
    npcShip.skyX = shipData.skyX
    npcShip.skyY = shipData.skyY
    npcShip.speed = shipData.speed.ShipSpeed
    npcShip.upgradeModule = shipData.upgradeModule - 1
    npcShip.destinationX = shipData.destinationX
    npcShip.destinationY = shipData.destinationY
    npcShip.repairModule = shipData.repairModule - 1
    npcShip.description = $shipData.description
    npcShip.homeBase = shipData.homeBase

proc getAdaShipModules(modules: array[1..75, AdaModuleData];
    getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  if getPlayerShip == 1:
    playerShip.modules = @[]
  else:
    npcShip.modules = @[]
  for adaModule in modules:
    if adaModule.mType == -1:
      return
    var module: ModuleData
    case adaModule.mType.ModuleType2
    of ModuleType2.engine:
      module = ModuleData(mType: ModuleType2.engine,
          fuelUsage: adaModule.data[1], power: adaModule.data[2],
          disabled: adaModule.data[3] == 1)
    of ModuleType2.cabin:
      module = ModuleData(mType: ModuleType2.cabin, cleanliness: adaModule.data[
          1], quality: adaModule.data[2])
    of ModuleType2.turret:
      module = ModuleData(mType: ModuleType2.turret, gunIndex: adaModule.data[
          1] - 1)
    of ModuleType2.gun:
      module = ModuleData(mType: ModuleType2.gun, damage: adaModule.data[1],
          ammoIndex: adaModule.data[2] - 1)
    of ModuleType2.hull:
      module = ModuleData(mType: ModuleType2.hull,
          installedModules: adaModule.data[1], maxModules: adaModule.data[2])
    of ModuleType2.workshop:
      module = ModuleData(mType: ModuleType2.workshop,
          craftingIndex: $adaModule.data2, craftingTime: adaModule.data[1],
              craftingAmount: adaModule.data[2])
    of ModuleType2.trainingRoom:
      module = ModuleData(mType: ModuleType2.trainingRoom,
          trainedSkill: adaModule.data[1])
    of ModuleType2.batteringRam:
      module = ModuleData(mType: ModuleType2.batteringRam,
          damage2: adaModule.data[1], coolingDown: adaModule.data[2] == 1)
    of ModuleType2.harpoonGun:
      module = ModuleData(mType: ModuleType2.harpoonGun,
          duration: adaModule.data[1], harpoonIndex: adaModule.data[2] - 1)
    of ModuleType2.any:
      module = ModuleData(mType: ModuleType2.any, data: [1: adaModule.data[
          1].int, 2: adaModule.data[2].int, 3: adaModule.data[3].int])
    of ModuleType2.cargoRoom:
      module = ModuleData(mType: ModuleType2.cargoRoom)
    of ModuleType2.medicalRoom:
      module = ModuleData(mType: ModuleType2.medicalRoom)
    of ModuleType2.cockpit:
      module = ModuleData(mType: ModuleType2.cockpit)
    of ModuleType2.armor:
      module = ModuleData(mType: ModuleType2.armor)
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
      module.owner.add(y = owner - 1)
    if module.owner.len == 0:
      module.owner.add(y = -1)
    if getPlayerShip == 1:
      playerShip.modules.add(y = module)
    else:
      npcShip.modules.add(y = module)

proc getAdaShipCargo(cargo: array[1..128, AdaInventoryData];
    getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  if getPlayerShip == 1:
    playerShip.cargo = @[]
  else:
    npcShip.cargo = @[]
  for adaItem in cargo:
    if adaItem.protoIndex == 0:
      return
    if getPlayerShip == 1:
      playerShip.cargo.add(y = InventoryData(protoIndex: adaItem.protoIndex,
          amount: adaItem.amount, name: $adaItem.name,
          durability: adaItem.durability, price: adaItem.price))
    else:
      npcShip.cargo.add(y = InventoryData(protoIndex: adaItem.protoIndex,
          amount: adaItem.amount, name: $adaItem.name,
          durability: adaItem.durability, price: adaItem.price))

proc setAdaShipCargo(cargo: var array[1..128, AdaInventoryData];
    getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  let nimCargo = if getPlayerShip == 1:
      playerShip.cargo
    else:
      npcShip.cargo
  for index in cargo.low..cargo.high:
    if index <= nimCargo.len:
      cargo[index] = AdaInventoryData(protoIndex: nimCargo[index -
          1].protoIndex.cint, amount: nimCargo[index - 1].amount.cint,
          name: nimCargo[index - 1].name.cstring, durability: nimCargo[index -
          1].durability.cint, price: nimCargo[index - 1].price.cint)
    else:
      cargo[index] = AdaInventoryData(protoIndex: 0)

func adaMemberToNim*(adaMember: AdaMemberData): MemberData {.raises: [], tags: [].} =
  result = MemberData(name: $adaMember.name, gender: adaMember.gender,
      health: adaMember.health, tired: adaMember.tired,
      hunger: adaMember.hunger, thirst: adaMember.thirst,
      order: adaMember.order.CrewOrders,
      previousOrder: adaMember.previousOrder.CrewOrders,
      contractLength: adaMember.contractLength, loyalty: adaMember.loyalty,
      homeBase: adaMember.homeBase, faction: $adaMember.faction)
  for index, order in adaMember.orders.pairs:
    result.orders[index] = order
  for index, item in adaMember.equipment.pairs:
    result.equipment[index.EquipmentLocations] = item - 1
  for attribute in adaMember.attributes:
    if attribute[0] == 0:
      break
    result.attributes.add(y = MobAttributeRecord(level: attribute[0],
        experience: attribute[1]))
  for skill in adaMember.skills:
    if skill[0] == 0:
      break
    result.skills.add(y = SkillInfo(index: skill[0], level: skill[1],
        experience: skill[2]))
  result.payment = [adaMember.payment[1].Natural, adaMember.payment[2].Natural]
  result.morale = [adaMember.morale[1].Natural, adaMember.morale[2].Natural]

proc getAdaShipCrew(crew: array[1..128, AdaMemberData];
    getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  if getPlayerShip == 1:
    playerShip.crew = @[]
  else:
    npcShip.crew = @[]
  for adaMember in crew:
    if adaMember.name.len == 0:
      return
    if getPlayerShip == 1:
      playerShip.crew.add(y = adaMemberToNim(adaMember = adaMember))
    else:
      npcShip.crew.add(y = adaMemberToNim(adaMember = adaMember))

proc getAdaCrewInventory(inventory: array[1..128, AdaInventoryData];
    memberIndex: cint; getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  if getPlayerShip == 1:
    playerShip.crew[memberIndex - 1].inventory = @[]
  else:
    npcShip.crew[memberIndex - 1].inventory = @[]
  for adaItem in inventory:
    if adaItem.protoIndex == 0:
      return
    if getPlayerShip == 1:
      playerShip.crew[memberIndex - 1].inventory.add(y = InventoryData(
          protoIndex: adaItem.protoIndex, amount: adaItem.amount,
          name: $adaItem.name,
          durability: adaItem.durability, price: adaItem.price))
    else:
      npcShip.crew[memberIndex - 1].inventory.add(y = InventoryData(
          protoIndex: adaItem.protoIndex, amount: adaItem.amount,
          name: $adaItem.name,
          durability: adaItem.durability, price: adaItem.price))

proc setAdaCrewInventory(inventory: var array[1..128, AdaInventoryData];
    memberIndex: cint; getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  let nimInventory = if getPlayerShip == 1:
      playerShip.crew[memberIndex - 1].inventory
    else:
      npcShip.crew[memberIndex - 1].inventory
  for index in inventory.low..inventory.high:
    if index <= nimInventory.len:
      inventory[index] = AdaInventoryData(protoIndex: nimInventory[index -
          1].protoIndex.cint, amount: nimInventory[index - 1].amount.cint,
          name: nimInventory[index - 1].name.cstring, durability: nimInventory[
          index - 1].durability.cint, price: nimInventory[index - 1].price.cint)
    else:
      inventory[index] = AdaInventoryData(protoIndex: 0)

func adaMemberFromNim*(member: MemberData): AdaMemberData {.raises: [], tags: [].} =
  result = AdaMemberData()
  for attribute in result.attributes.mitems:
    attribute = [0.cint, 0.cint]
  for index, attribute in member.attributes.pairs:
    result.attributes[index + 1] = [attribute.level.cint, attribute.experience.cint]
  for skill in result.skills.mitems:
    skill = [0.cint, 0.cint, 0.cint]
  for index, skill in member.skills.pairs:
    result.skills[index + 1] = [skill.index.cint, skill.level.cint,
        skill.experience.cint]
  result.name = member.name.cstring
  result.gender = member.gender
  result.health = member.health.cint
  result.tired = member.tired.cint
  result.hunger = member.hunger.cint
  result.thirst = member.thirst.cint
  result.order = member.order.ord.cint
  result.previousOrder = member.previousOrder.ord.cint
  result.orderTime = member.orderTime.cint
  for index, order in member.orders.pairs:
    result.orders[index] = order.ord.cint
  for index, item in member.equipment:
    result.equipment[index.ord.cint] = item.cint
  result.payment = [1: member.payment[1].cint, 2: member.payment[2].cint]
  result.contractLength = member.contractLength.cint
  result.morale = [1: member.morale[1].cint, 2: member.morale[2].cint]
  result.loyalty = member.loyalty.cint
  result.homeBase = member.homeBase
  result.faction = member.faction.cstring

proc setAdaShipCrew(crew: var array[1..128, AdaMemberData];
    getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  let nimCrew = if getPlayerShip == 1:
      playerShip.crew
    else:
      npcShip.crew
  var index = 1
  for member in nimCrew:
    crew[index] = adaMemberFromNim(member = member)
    index.inc
  crew[index].name = "".cstring

proc setAdaShip(shipData: var AdaShipData; getPlayerShip: cint = 1) {.raises: [
    ], tags: [], exportc.} =
  let nimShip = if getPlayerShip == 1:
      playerShip
    else:
      npcShip
  shipData.name = nimShip.name.cstring
  shipData.skyX = nimShip.skyX.cint
  shipData.skyY = nimShip.skyY.cint
  shipData.speed = nimShip.speed.ord.cint
  shipData.upgradeModule = nimShip.upgradeModule.cint + 1
  shipData.destinationX = nimShip.destinationX.cint
  shipData.destinationY = nimShip.destinationY.cint
  shipData.repairModule = nimShip.repairModule.cint + 1
  shipData.description = nimShip.description.cstring
  shipData.homeBase = nimShip.homeBase.cint

proc setAdaShipModules(modules: var array[1..75, AdaModuleData];
    getPlayerShip: cint = 1) {.raises: [], tags: [], exportc.} =
  for i in modules.low..modules.high:
    modules[i] = AdaModuleData(name: "".cstring)
  let nimModules = if getPlayerShip == 1:
      playerShip.modules
    else:
      npcShip.modules
  var index = 1
  for module in nimModules:
    var
      adaModule = AdaModuleData(name: module.name.cstring,
          protoIndex: module.protoIndex.cint, weight: module.weight.cint,
          durability: module.durability.cint,
          maxDurability: module.maxDurability.cint,
          upgradeProgress: module.upgradeProgress.cint,
          upgradeAction: module.upgradeAction.ord.cint,
          mType: module.mType.ModuleType2.ord.cint)
      secondIndex = 1
    case module.mType
    of ModuleType2.engine:
      adaModule.data = [module.fuelUsage.cint, module.power.cint, (
          if module.disabled: 1 else: 0)]
    of ModuleType2.cabin:
      adaModule.data = [module.cleanliness.cint, module.quality.cint, 0]
    of ModuleType2.turret:
      adaModule.data = [(module.gunIndex + 1).cint, 0.cint, 0.cint]
    of ModuleType2.gun:
      adaModule.data = [module.damage.cint, (module.ammoIndex + 1).cint, 0.cint]
    of ModuleType2.hull:
      adaModule.data = [module.installedModules.cint, module.maxModules.cint, 0.cint]
    of ModuleType2.workshop:
      adaModule.data = [module.craftingTime.cint, module.craftingAmount.cint, 0.cint]
      adaModule.data2 = module.craftingIndex.cstring
    of ModuleType2.trainingRoom:
      adaModule.data = [module.trainedSkill.cint, 0.cint, 0.cint]
    of ModuleType2.batteringRam:
      adaModule.data = [module.damage2.cint, (if module.coolingDown: 1 else: 0), 0.cint]
    of ModuleType2.harpoonGun:
      adaModule.data = [module.duration.cint, (module.harpoonIndex + 1).cint, 0.cint]
    else:
      discard
    for owner in module.owner:
      adaModule.owner[secondIndex] = (owner + 1).cint
      secondIndex.inc
    modules[index] = adaModule
    index.inc
