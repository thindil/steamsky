# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides code related to saving and loading the player's ship's data from
## a file.

import std/[strutils, tables, xmltree]
import contracts
import game, types

proc savePlayerShip*(saveData: var XmlNode) {.raises: [], tags: [],
    contractual.} =
  ## Save the player's ship data from the current game into a file
  ##
  ## * saveData - the XML structure to which the ship will be saved
  var shipTree: XmlNode = newXmlTree(tag = "playership", children = [],
      attributes = {"name": playerShip.name, "x": $playerShip.skyX,
      "y": $playerShip.skyY,
      "speed": $playerShip.speed.ord,
      "upgrademodule": $(playerShip.upgradeModule + 1),
      "destinationx": $playerShip.destinationX,
      "destinationy": $playerShip.destinationY,
      "repairpriority": $(playerShip.repairModule + 1),
      "homebase": $playerShip.homeBase}.toXmlAttributes)
  for module in playerShip.modules:
    var attrs: seq[tuple[key, val: string]] = @[("name", module.name), ("index",
        $module.protoIndex), ("weight", $module.weight), ("durability",
        $module.durability), ("maxdurability", $module.maxDurability)]
    if module.upgradeProgress > 0:
      attrs.add(y = ("upgradeprogress", $module.upgradeProgress))
    if module.upgradeAction != none:
      attrs.add(y = ("upgradeaction", $module.upgradeAction.ord))
    var moduleTree: XmlNode = newXmlTree(tag = "module", children = [],
        attributes = attrs.toXmlAttributes)
    for owner in module.owner:
      var ownerElement: XmlNode = newElement(tag = "owner")
      ownerElement.attrs = {"value": $(owner + 1)}.toXmlAttributes
      moduleTree.add(son = ownerElement)
    case module.mType
    of ModuleType2.workshop:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": module.craftingIndex}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.craftingTime}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.craftingAmount}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.craftingQuality.ord)}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.craftingBonus.ord)}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.craftingMalus.ord)}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.trainingRoom:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $module.trainedSkill}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.medicalRoom, ModuleType2.cockpit, ModuleType2.armor,
        ModuleType2.any, ModuleType2.cargoRoom:
      discard
    of ModuleType2.engine:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $module.fuelUsage}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.power}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": (if module.disabled: "1" else: "0")}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.cabin:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $module.cleanliness}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.quality}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.turret:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.gunIndex + 1)}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.gun:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.ammoIndex + 1)}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.damage}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.hull:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $module.installedModules}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.maxModules}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.batteringRam:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $module.damage2}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.harpoonGun:
      var dataElement: XmlNode = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.harpoonIndex + 1)}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.duration}.toXmlAttributes
      moduleTree.add(son = dataElement)
    shipTree.add(son = moduleTree)
  for item in playerShip.cargo:
    var attrs: seq[tuple[key, val: string]] = @[("index", $item.protoIndex), (
        "amount", $item.amount), ("durability", $item.durability)]
    if item.name.len > 0:
      attrs.add(y = ("name", item.name))
    if item.price > 0:
      attrs.add(y = ("price", $item.price))
    if item.quality != normal:
      attrs.add(y = ("quality", $item.quality))
    if item.maxDurability != 100:
      attrs.add(y = ("maxdurability", $item.maxDurability))
    var itemElement: XmlNode = newElement(tag = "cargo")
    itemElement.attrs = attrs.toXmlAttributes
    shipTree.add(son = itemElement)
  const attributesNames: array[14, string] = ["health", "tired", "hunger",
      "thirst", "order", "previousorder", "ordertime", "dailypay", "tradepay",
      "contractlength",
      "moralelevel", "moralepoints", "loyalty", "homebase"]
  for member in playerShip.crew:
    var attrs2: seq[tuple[key, val: string]] = @[]
    let values: array[14, int] = [member.health, member.tired, member.hunger,
        member.thirst, member.order.ord, member.previousOrder.ord,
        member.orderTime,
        member.payment[1], member.payment[2], member.contractLength,
        member.morale[1], member.morale[2], member.loyalty, member.homeBase]
    for index, name in attributesNames:
      attrs2.add(y = (name, $values[index]))
    attrs2.add(y = ("name", member.name))
    attrs2.add(y = ("gender", $member.gender))
    attrs2.add(y = ("faction", member.faction))
    var memberTree: XmlNode = newXmlTree(tag = "member", children = [],
        attributes = attrs2.toXmlAttributes)
    for skill in member.skills:
      var skillElement: XmlNode = newElement(tag = "skill")
      skillElement.attrs = {"index": $skill.index, "level": $skill.level,
          "experience": $skill.experience}.toXmlAttributes
      memberTree.add(son = skillElement)
    for priority in member.orders:
      var priorityElement: XmlNode = newElement(tag = "priority")
      priorityElement.attrs = {"value": $priority}.toXmlAttributes
      memberTree.add(son = priorityElement)
    for attribute in member.attributes:
      var attributeElement: XmlNode = newElement(tag = "attribute")
      attributeElement.attrs = {"level": $attribute.level,
          "experience": $attribute.experience}.toXmlAttributes
      memberTree.add(son = attributeElement)
    for item in member.inventory:
      var
        itemElement: XmlNode = newElement(tag = "item")
        attrs: seq[tuple[key, val: string]] = @[("index", $item.protoIndex), (
            "amount", $item.amount), ("durability", $item.durability)]
      if item.name.len > 0:
        attrs.add(y = ("name", item.name))
      if item.price > 0:
        attrs.add(y = ("price", $item.price))
      itemElement.attrs = attrs.toXmlAttributes
      memberTree.add(son = itemElement)
    for item in member.equipment:
      var itemElement: XmlNode = newElement(tag = "equipment")
      itemElement.attrs = {"index": $(item + 1)}.toXmlAttributes
      memberTree.add(son = itemElement)
    shipTree.add(son = memberTree)
  saveData.add(son = shipTree)

proc loadPlayerShip*(saveData: XmlNode) {.raises: [ValueError],
    tags: [], contractual.} =
  ## Load the player's ship data from the file
  ##
  ## * saveData - the XML structure from which the ship will be loaded
  require:
    saveData != nil
  body:
    let shipNode: XmlNode = saveData.child(name = "playership")
    playerShip.name = shipNode.attr(name = "name")
    playerShip.skyX = shipNode.attr(name = "x").parseInt
    playerShip.skyY = shipNode.attr(name = "y").parseInt
    playerShip.speed = shipNode.attr(name = "speed").parseInt.ShipSpeed
    playerShip.upgradeModule = shipNode.attr(name = "upgrademodule").parseInt - 1
    playerShip.destinationX = shipNode.attr(name = "destinationx").parseInt
    playerShip.destinationY = shipNode.attr(name = "destinationy").parseInt
    playerShip.repairModule = shipNode.attr(name = "repairpriority").parseInt - 1
    playerShip.homeBase = shipNode.attr(name = "homebase").parseInt
    playerShip.modules = @[]
    playerShip.cargo = @[]
    playerShip.crew = @[]
    for module in shipNode.findAll(tag = "module"):
      let
        name: string = module.attr(name = "name")
        protoIndex: int = module.attr(name = "index").parseInt
        weight: Positive = module.attr(name = "weight").parseInt
        modDur: Natural = module.attr(name = "durability").parseInt
        maxDur: Positive = module.attr(name = "maxdurability").parseInt
      var
        owners: seq[int] = @[]
        upgradeAction: ShipUpgrade = none
        upgradeProgress: int = 0
        mType: ModuleType2 = ModuleType2.any
      if module.attr(name = "owner") == "":
        for owner in module.findAll(tag = "owner"):
          owners.add(y = owner.attr(name = "value").parseInt - 1)
      else:
        owners.add(y = module.attr(name = "owner").parseInt - 1)
      if module.attr(name = "upgradeaction") != "":
        upgradeAction = module.attr(name = "upgradeaction").parseInt.ShipUpgrade
      if module.attr(name = "upgradeprogress") != "":
        upgradeProgress = module.attr(name = "upgradeprogress").parseInt
      if module.attr(name = "mtype") == "":
        case modulesList[protoIndex].mType
        of alchemyLab..greenhouse:
          mType = workshop
        of medicalRoom:
          mType = medicalRoom
        of trainingRoom:
          mType = trainingRoom
        of engine:
          mType = engine
        of cabin:
          mType = cabin
        of cockpit:
          mType = cockpit
        of turret:
          mType = turret
        of gun:
          mType = gun
        of cargo:
          mType = cargoRoom
        of hull:
          mType = hull
        of armor:
          mType = armor
        of batteringRam:
          mType = batteringRam
        of harpoonGun:
          mType = harpoonGun
        else:
          mType = ModuleType2.any
      else:
        case modulesList[protoIndex].mType
        of medicalRoom:
          mType = medicalRoom
        of trainingRoom:
          mType = trainingRoom
        of engine:
          mType = engine
        of cabin:
          mType = cabin
        of cockpit:
          mType = cockpit
        of turret:
          mType = turret
        of gun:
          mType = gun
        of cargo:
          mType = cargoRoom
        of hull:
          mType = hull
        of armor:
          mType = armor
        of batteringRam:
          mType = batteringRam
        of harpoonGun:
          mType = harpoonGun
        else:
          mType = parseEnum[ModuleType2](s = module.attr(name = "mtype"))
      var
        data: array[1..3, int] = [0, 0, 0]
        dataIndex: int = 1
      case mType
      of ModuleType2.any:
        for modData in module.findAll(tag = "data"):
          data[dataIndex] = modData.attr(name = "value").parseInt
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: ModuleType2.any,
            name: name, protoIndex: protoIndex, weight: weight,
            durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction, data: data))
      of engine:
        var
          fuelUsage, power: int = 0
          disabled: bool = false
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            fuelUsage = modData.attr(name = "value").parseInt
          of 2:
            power = modData.attr(name = "value").parseInt
          of 3:
            disabled = modData.attr(name = "value") == "1"
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: engine, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            fuelUsage: fuelUsage, power: power, disabled: disabled))
      of cabin:
        var cleanliness, quality: int = 0
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            cleanliness = modData.attr(name = "value").parseInt
          of 2:
            quality = modData.attr(name = "value").parseInt
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: cabin, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            cleanliness: cleanliness, quality: quality))
      of cockpit:
        playerShip.modules.add(y = ModuleData(mType: cockpit, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
      of workshop:
        var
          craftingIndex: string = ""
          craftingTime, craftingAmount: int = 0
          craftingQuality: ObjectQuality = normal
          craftingBonus: CraftBonuses = none
          craftingMalus: CraftMaluses = none
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            craftingIndex = modData.attr(name = "value")
            if craftingIndex == "0":
              craftingIndex = ""
          of 2:
            craftingTime = modData.attr(name = "value").parseInt
          of 3:
            craftingAmount = modData.attr(name = "value").parseInt
          of 4:
            craftingQuality = modData.attr(
                name = "value").parseInt.ObjectQuality
          of 5:
            craftingBonus = modData.attr(name = "value").parseInt.CraftBonuses
          of 6:
            craftingMalus = modData.attr(name = "value").parseInt.CraftMaluses
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: workshop, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            craftingIndex: craftingIndex, craftingTime: craftingTime,
            craftingAmount: craftingAmount, craftingQuality: craftingQuality,
            craftingBonus: craftingBonus, craftingMalus: craftingMalus))
      of medicalRoom:
        playerShip.modules.add(y = ModuleData(mType: medicalRoom, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
      of trainingRoom:
        var trainedSkill: int = 0
        for modData in module.findAll(tag = "data"):
          if dataIndex == 1:
            trainedSkill = modData.attr(name = "value").parseInt
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: trainingRoom, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            trainedSkill: trainedSkill))
      of turret:
        var gunIndex: int = -1
        for modData in module.findAll(tag = "data"):
          if dataIndex == 1:
            gunIndex = modData.attr(name = "value").parseInt - 1
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: turret, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            gunIndex: gunIndex))
      of gun:
        var
          damage: int = 0
          ammoIndex: int = -1
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            ammoIndex = modData.attr(name = "value").parseInt - 1
          of 2:
            damage = modData.attr(name = "value").parseInt
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: gun, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            damage: damage, ammoIndex: ammoIndex))
      of cargoRoom:
        playerShip.modules.add(y = ModuleData(mType: cargoRoom, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
      of hull:
        var installedModules, maxModules: int = 0
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            installedModules = modData.attr(name = "value").parseInt
          of 2:
            maxModules = modData.attr(name = "value").parseInt
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: hull, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            installedModules: installedModules, maxModules: maxModules))
      of armor:
        playerShip.modules.add(y = ModuleData(mType: armor, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
      of batteringRam:
        var damage: Natural = 0
        for modData in module.findAll(tag = "data"):
          if dataIndex == 1:
            damage = modData.attr(name = "value").parseInt
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: batteringRam, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            damage2: damage, coolingDown: false))
      of harpoonGun:
        var
          duration: Natural = 0
          harpoonIndex: int = -1
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            harpoonIndex = modData.attr(name = "value").parseInt - 1
          of 2:
            duration = modData.attr(name = "value").parseInt
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: harpoonGun, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            duration: duration, harpoonIndex: harpoonIndex))
    for cargo in shipNode.findAll(tag = "cargo"):
      let
        protoIndex: int = cargo.attr(name = "index").parseInt
        amount: Natural = cargo.attr(name = "amount").parseInt
        name: string = cargo.attr(name = "name")
        itemDurability: int = cargo.attr(name = "durability").parseInt
        price: int = (if cargo.attr(name = "price").len ==
            0: 0 else: cargo.attr(name = "price").parseInt)
        quality: ObjectQuality = (if cargo.attr(name = "quality").len ==
            0: normal else: parseEnum[ObjectQuality](s = cargo.attr(
            name = "quality")))
        maxDurability: ItemsDurability = (if cargo.attr(
            name = "maxdurability").len == 0: 100 else: cargo.attr(
            name = "maxdurability").parseInt)
      playerShip.cargo.add(y = InventoryData(protoIndex: protoIndex,
          amount: amount, name: name, durability: itemDurability, price: price,
          quality: quality, maxDurability: maxDurability))
    for crew in shipNode.findAll(tag = "member"):
      var member: MemberData = MemberData()
      member.name = crew.attr(name = "name")
      member.gender = crew.attr(name = "gender")[0]
      member.health = crew.attr(name = "health").parseInt
      member.tired = crew.attr(name = "tired").parseInt
      member.hunger = crew.attr(name = "hunger").parseInt
      member.thirst = crew.attr(name = "thirst").parseInt
      member.order = crew.attr(name = "order").parseInt.CrewOrders
      member.previousOrder = crew.attr(name = "previousorder").parseInt.CrewOrders
      member.orderTime = crew.attr(name = "ordertime").parseInt
      member.payment[1] = crew.attr(name = "dailypay").parseInt
      member.payment[2] = crew.attr(name = "tradepay").parseInt
      member.contractLength = crew.attr(name = "contractlength").parseInt
      member.morale[1] = crew.attr(name = "moralelevel").parseInt
      member.morale[2] = crew.attr(name = "moralepoints").parseInt
      member.loyalty = crew.attr(name = "loyalty").parseInt
      for skill in crew.findAll(tag = "skill"):
        let
          index: int = skill.attr(name = "index").parseInt
          level: Natural = skill.attr(name = "level").parseInt
          experience: Natural = (if skill.attr(name = "experience").len >
              0: skill.attr(name = "experience").parseInt else: 0)
        member.skills.add(y = SkillInfo(index: index, level: level,
            experience: experience))
      var priorityIndex: int = 1
      for priority in crew.findAll(tag = "priority"):
        member.orders[priorityIndex] = priority.attr(name = "value").parseInt
        priorityIndex.inc
      for attribute in crew.findAll(tag = "attribute"):
        let
          level: int = attribute.attr(name = "level").parseInt
          experience: Natural = (if attribute.attr(name = "experience").len >
              0: attribute.attr(name = "experience").parseInt else: 0)
        member.attributes.add(y = MobAttributeRecord(level: level,
            experience: experience))
      for item in crew.findAll(tag = "item"):
        let
          itemIndex: int = item.attr(name = "index").parseInt
          amount: Natural = item.attr(name = "amount").parseInt
          itemName: string = item.attr(name = "name")
          itemDurability: int = item.attr(name = "durability").parseInt
          price: int = (if item.attr(name = "price").len > 0: item.attr(
              name = "price").parseInt else: 0)
        member.inventory.add(y = InventoryData(protoIndex: itemIndex,
            amount: amount, name: itemName, durability: itemDurability, price: price))
      var equipmentIndex: int = 1
      for item in crew.findAll(tag = "equipment"):
        member.equipment[(equipmentIndex - 1).EquipmentLocations] = item.attr(
            name = "index").parseInt - 1
        equipmentIndex.inc
      member.homeBase = (if crew.attr(name = "homebase").len > 0: crew.attr(
          name = "homebase").parseInt else: playerShip.homeBase)
      member.faction = (if crew.attr(name = "faction").len > 0: crew.attr(
          name = "faction") else: skyBases[member.homeBase].owner)
      playerShip.crew.add(y = member)
