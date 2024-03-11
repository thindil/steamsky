# Copyright 2023-2024 Bartek thindil Jasicki
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

import std/[strutils, tables, xmltree]
import contracts
import game, types

proc savePlayerShip*(saveData: var XmlNode) {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Save the player's ship data from the current game into a file
  ##
  ## * saveData - the XML structure to which the ship will be saved
  var shipTree = newXmlTree(tag = "playership", children = [], attributes = {
      "name": playerShip.name, "x": $playerShip.skyX, "y": $playerShip.skyY,
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
    var moduleTree = newXmlTree(tag = "module", children = [],
        attributes = attrs.toXmlAttributes)
    for owner in module.owner:
      var ownerElement = newElement(tag = "owner")
      ownerElement.attrs = {"value": $(owner + 1)}.toXmlAttributes
      moduleTree.add(son = ownerElement)
    case module.mType
    of ModuleType2.workshop:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": module.craftingIndex}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.craftingTime}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.craftingAmount}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.trainingRoom:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.trainedSkill}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.medicalRoom, ModuleType2.cockpit, ModuleType2.armor,
        ModuleType2.any, ModuleType2.cargoRoom:
      discard
    of ModuleType2.engine:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.fuelUsage}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.power}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": (if module.disabled: "1" else: "0")}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.cabin:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.cleanliness}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.quality}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.turret:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.gunIndex + 1)}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.gun:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $(module.ammoIndex + 1)}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.damage}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.hull:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.installedModules}.toXmlAttributes
      moduleTree.add(son = dataElement)
      dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.maxModules}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.batteringRam:
      var dataElement = newElement(tag = "data")
      dataElement.attrs = {"value": $module.damage2}.toXmlAttributes
      moduleTree.add(son = dataElement)
    of ModuleType2.harpoonGun:
      var dataElement = newElement(tag = "data")
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
    var itemElement = newElement(tag = "cargo")
    itemElement.attrs = attrs.toXmlAttributes
    shipTree.add(son = itemElement)
  const attributesNames = ["health", "tired", "hunger", "thirst", "order",
      "previousorder", "ordertime", "dailypay", "tradepay", "contractlength",
      "moralelevel", "moralepoints", "loyalty", "homebase"]
  for member in playerShip.crew:
    var attrs: seq[tuple[key, val: string]] = @[]
    let values = [member.health, member.tired, member.hunger, member.thirst,
        member.order.ord, member.previousOrder.ord, member.orderTime,
        member.payment[1], member.payment[2], member.contractLength,
        member.morale[1], member.morale[2], member.loyalty, member.homeBase]
    for index, name in attributesNames.pairs:
      attrs.add(y = (name, $values[index]))
    attrs.add(y = ("name", member.name))
    attrs.add(y = ("gender", $member.gender))
    attrs.add(y = ("faction", member.faction))
    var memberTree = newXmlTree(tag = "member", children = [],
        attributes = attrs.toXmlAttributes)
    for skill in member.skills:
      var skillElement = newElement(tag = "skill")
      skillElement.attrs = {"index": $skill.index, "level": $skill.level,
          "experience": $skill.experience}.toXmlAttributes
      memberTree.add(son = skillElement)
    for priority in member.orders:
      var priorityElement = newElement(tag = "priority")
      priorityElement.attrs = {"value": $priority}.toXmlAttributes
      memberTree.add(son = priorityElement)
    for attribute in member.attributes:
      var attributeElement = newElement(tag = "attribute")
      attributeElement.attrs = {"level": $attribute.level,
          "experience": $attribute.experience}.toXmlAttributes
      memberTree.add(son = attributeElement)
    for item in member.inventory:
      var
        itemElement = newElement(tag = "item")
        attrs: seq[tuple[key, val: string]] = @[("index", $item.protoIndex), (
            "amount", $item.amount), ("durability", $item.durability)]
      if item.name.len > 0:
        attrs.add(y = ("name", item.name))
      if item.price > 0:
        attrs.add(y = ("price", $item.price))
      itemElement.attrs = attrs.toXmlAttributes
      memberTree.add(son = itemElement)
    for item in member.equipment:
      var itemElement = newElement(tag = "equipment")
      itemElement.attrs = {"index": $(item + 1)}.toXmlAttributes
      memberTree.add(son = itemElement)
    shipTree.add(son = memberTree)
  saveData.add(son = shipTree)

proc loadPlayerShip*(saveData: XmlNode) {.sideEffect, raises: [ValueError],
    tags: [], contractual.} =
  ## Load the player's ship data from the file
  ##
  ## * saveData - the XML structure from which the ship will be loaded
  require:
    saveData != nil
  body:
    let shipNode = saveData.child(name = "playership")
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
        name = module.attr(name = "name")
        protoIndex = module.attr(name = "index").parseInt
        weight = module.attr(name = "weight").parseInt
        modDur = module.attr(name = "durability").parseInt
        maxDur = module.attr(name = "maxdurability").parseInt
      var
        owners: seq[int]
        upgradeAction: ShipUpgrade = none
        upgradeProgress = 0
        mType: ModuleType2 = any
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
        of alchemyLab .. greenhouse:
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
          mType = any
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
        dataIndex = 1
      case mType
      of any:
        for modData in module.findAll(tag = "data"):
          data[dataIndex] = modData.attr(name = "value").parseInt
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: any, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction, data: data))
      of engine:
        var
          fuelUsage, power = 0
          disabled = false
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
        var cleanliness, quality = 0
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
          craftingIndex = ""
          craftingTime, craftingAmount = 0
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
          else:
            discard
          dataIndex.inc
        playerShip.modules.add(y = ModuleData(mType: workshop, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
            craftingIndex: craftingIndex, craftingTime: craftingTime,
            craftingAmount: craftingAmount))
      of medicalRoom:
        playerShip.modules.add(y = ModuleData(mType: medicalRoom, name: name,
            protoIndex: protoIndex, weight: weight, durability: modDur,
            maxDurability: maxDur, owner: owners,
            upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
      of trainingRoom:
        var trainedSkill = 0
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
        var gunIndex = -1
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
          damage = 0
          ammoIndex = -1
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
        var installedModules, maxModules = 0
        for modData in module.findAll(tag = "data"):
          case dataIndex
          of 1:
            installedModules = modData.attr(name = "value").parseInt - 1
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
        var damage = 0
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
          duration = 0
          harpoonIndex = -1
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
        protoIndex = cargo.attr(name = "index").parseInt
        amount = cargo.attr(name = "amount").parseInt
        name = cargo.attr(name = "name")
        itemDurability = cargo.attr(name = "durability").parseInt
        price = (if cargo.attr(name = "price").len == 0: 0 else: cargo.attr(
            name = "price").parseInt)
      playerShip.cargo.add(y = InventoryData(protoIndex: protoIndex, amount: amount,
          name: name, durability: itemDurability, price: price))
    for crew in shipNode.findAll(tag = "member"):
      var member: MemberData
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
          index = skill.attr("index").parseInt
          level = skill.attr("level").parseInt
          experience = (if skill.attr("experience").len > 0: skill.attr(
              "experience").parseInt else: 0)
        member.skills.add(SkillInfo(index: index, level: level,
            experience: experience))
      var priorityIndex = 1
      for priority in crew.findAll("priority"):
        member.orders[priorityIndex] = priority.attr("value").parseInt
        priorityIndex.inc
      for attribute in crew.findAll("attribute"):
        let
          level = attribute.attr("level").parseInt
          experience = (if attribute.attr("experience").len > 0: attribute.attr(
              "experience").parseInt else: 0)
        member.attributes.add(MobAttributeRecord(level: level,
            experience: experience))
      for item in crew.findAll("item"):
        let
          itemIndex = item.attr("index").parseInt
          amount = item.attr("amount").parseInt
          itemName = item.attr("name")
          itemDurability = item.attr("durability").parseInt
          price = (if item.attr("price").len > 0: item.attr(
              "price").parseInt else: 0)
        member.inventory.add(InventoryData(protoIndex: itemIndex,
            amount: amount, name: itemName, durability: itemDurability, price: price))
      var equipmentIndex = 1
      for item in crew.findAll("equipment"):
        member.equipment[(equipmentIndex - 1).EquipmentLocations] = item.attr(
            "index").parseInt - 1
        equipmentIndex.inc
      member.homeBase = (if crew.attr("homebase").len > 0: crew.attr(
          "homebase").parseInt else: playerShip.homeBase)
      member.faction = (if crew.attr("faction").len > 0: crew.attr(
          "faction") else: skyBases[member.homeBase].owner)
      playerShip.crew.add(member)
