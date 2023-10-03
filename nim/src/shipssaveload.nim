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

import std/[strutils, tables, xmltree]
import game, types

proc savePlayerShip*(saveData: var XmlNode) {.sideEffect, raises: [], tags: [].} =
  ## Save the player's ship data from the current game into a file
  ##
  ## * saveData - the XML structure to which the ship will be saved
  var shipTree = newXmlTree("playership", [], {"name": playerShip.name,
      "x": $playerShip.skyX, "y": $playerShip.skyY,
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
      attrs.add(("upgradeprogress", $module.upgradeProgress))
    if module.upgradeAction != none:
      attrs.add(("upgradeaction", $module.upgradeAction.ord))
    var moduleTree = newXmlTree("module", [], attrs.toXmlAttributes)
    for owner in module.owner:
      var ownerElement = newElement("owner")
      ownerElement.attrs = {"value": $(owner + 1)}.toXmlAttributes
      moduleTree.add(ownerElement)
    case module.mType
    of ModuleType2.workshop:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": module.craftingIndex}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.craftingTime}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.craftingAmount}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.trainingRoom:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $module.trainedSkill}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.medicalRoom, ModuleType2.cockpit, ModuleType2.armor,
        ModuleType2.any, ModuleType2.cargoRoom:
      discard
    of ModuleType2.engine:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $module.fuelUsage}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.power}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": (if module.disabled: "1" else: "0")}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.cabin:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $module.cleanliness}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.quality}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.turret:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $(module.gunIndex + 1)}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.gun:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $(module.ammoIndex + 1)}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.damage}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.hull:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $module.installedModules}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.maxModules}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.batteringRam:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $module.damage2}.toXmlAttributes
      moduleTree.add(dataElement)
    of ModuleType2.harpoonGun:
      var dataElement = newElement("data")
      dataElement.attrs = {"value": $(module.harpoonIndex + 1)}.toXmlAttributes
      moduleTree.add(dataElement)
      dataElement = newElement("data")
      dataElement.attrs = {"value": $module.duration}.toXmlAttributes
      moduleTree.add(dataElement)
    shipTree.add(moduleTree)
  for item in playerShip.cargo:
    var attrs: seq[tuple[key, val: string]] = @[("index", $item.protoIndex), (
        "amount", $item.amount), ("durability", $item.durability)]
    if item.name.len > 0:
      attrs.add(("name", item.name))
    if item.price > 0:
      attrs.add(("price", $item.price))
    var itemElement = newElement("cargo")
    itemElement.attrs = attrs.toXmlAttributes
    shipTree.add(itemElement)
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
      attrs.add((name, $values[index]))
    attrs.add(("name", member.name))
    attrs.add(("gender", $member.gender))
    attrs.add(("faction", member.faction))
    var memberTree = newXmlTree("member", [], attrs.toXmlAttributes)
    for skill in member.skills:
      var skillElement = newElement("skill")
      skillElement.attrs = {"index": $skill.index, "level": $skill.level,
          "experience": $skill.experience}.toXmlAttributes
      memberTree.add(skillElement)
    for priority in member.orders:
      var priorityElement = newElement("priority")
      priorityElement.attrs = {"value": $priority}.toXmlAttributes
      memberTree.add(priorityElement)
    for attribute in member.attributes:
      var attributeElement = newElement("attribute")
      attributeElement.attrs = {"level": $attribute.level,
          "experience": $attribute.experience}.toXmlAttributes
      memberTree.add(attributeElement)
    for item in member.inventory:
      var
        itemElement = newElement("item")
        attrs: seq[tuple[key, val: string]] = @[("index", $item.protoIndex), (
            "amount", $item.amount), ("durability", $item.durability)]
      if item.name.len > 0:
        attrs.add(("name", item.name))
      if item.price > 0:
        attrs.add(("price", $item.price))
      itemElement.attrs = attrs.toXmlAttributes
      memberTree.add(itemElement)
    for item in member.equipment:
      var itemElement = newElement("equipment")
      itemElement.attrs = {"index": $(item + 1)}.toXmlAttributes
      memberTree.add(itemElement)
    shipTree.add(memberTree)
  saveData.add(shipTree)

proc loadPlayerShip*(saveData: XmlNode) {.sideEffect, raises: [ValueError],
    tags: [].} =
  ## Load the player's ship data from the file
  ##
  ## * saveData - the XML structure from which the ship will be loaded
  let shipNode = saveData.child("playership")
  playerShip.name = shipNode.attr("name")
  playerShip.skyX = shipNode.attr("x").parseInt
  playerShip.skyY = shipNode.attr("y").parseInt
  playerShip.speed = shipNode.attr("speed").parseInt.ShipSpeed
  playerShip.upgradeModule = shipNode.attr("upgrademodule").parseInt - 1
  playerShip.destinationX = shipNode.attr("destinationx").parseInt
  playerShip.destinationY = shipNode.attr("destinationy").parseInt
  playerShip.repairModule = shipNode.attr("repairpriority").parseInt - 1
  playerShip.homeBase = shipNode.attr("homebase").parseInt
  playerShip.modules = @[]
  playerShip.cargo = @[]
  playerShip.crew = @[]
  for module in shipNode.findAll("module"):
    let
      name = module.attr("name")
      protoIndex = module.attr("index").parseInt
      weight = module.attr("weight").parseInt
      modDur = module.attr("durability").parseInt
      maxDur = module.attr("maxdurability").parseInt
    var
      owners: seq[int]
      upgradeAction: ShipUpgrade = none
      upgradeProgress = 0
      mType: ModuleType2 = any
    if module.attr("owner") == "":
      for owner in module.findAll("owner"):
        owners.add(owner.attr("value").parseInt - 1)
    else:
      owners.add(module.attr("owner").parseInt - 1)
    if module.attr("upgradeaction") != "":
      upgradeAction = module.attr("upgradeaction").parseInt.ShipUpgrade
    if module.attr("upgradeprogress") != "":
      upgradeProgress = module.attr("upgradeprogress").parseInt
    if module.attr("mtype") == "":
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
        mType = parseEnum[ModuleType2](module.attr("mtype"))
    var
      data: array[1..3, int] = [0, 0, 0]
      dataIndex = 1
    case mType
    of any:
      for modData in module.findAll("data"):
        data[dataIndex] = modData.attr("value").parseInt
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: any, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction, data: data))
    of engine:
      var
        fuelUsage, power = 0
        disabled = false
      for modData in module.findAll("data"):
        case dataIndex
        of 1:
          fuelUsage = modData.attr("value").parseInt
        of 2:
          power = modData.attr("value").parseInt
        of 3:
          disabled = modData.attr("value") == "1"
        else:
          discard
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: engine, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          fuelUsage: fuelUsage, power: power, disabled: disabled))
    of cabin:
      var cleanliness, quality = 0
      for modData in module.findAll("data"):
        case dataIndex
        of 1:
          cleanliness = modData.attr("value").parseInt
        of 2:
          quality = modData.attr("value").parseInt
        else:
          discard
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: cabin, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          cleanliness: cleanliness, quality: quality))
    of cockpit:
      playerShip.modules.add(ModuleData(mType: cockpit, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
    of workshop:
      var
        craftingIndex = ""
        craftingTime, craftingAmount = 0
      for modData in module.findAll("data"):
        case dataIndex
        of 1:
          craftingIndex = modData.attr("value")
          if craftingIndex == "0":
            craftingIndex = ""
        of 2:
          craftingTime = modData.attr("value").parseInt
        of 3:
          craftingAmount = modData.attr("value").parseInt
        else:
          discard
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: workshop, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          craftingIndex: craftingIndex, craftingTime: craftingTime,
          craftingAmount: craftingAmount))
    of medicalRoom:
      playerShip.modules.add(ModuleData(mType: medicalRoom, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
    of trainingRoom:
      var trainedSkill = 0
      for modData in module.findAll("data"):
        if dataIndex == 1:
          trainedSkill = modData.attr("value").parseInt
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: trainingRoom, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          trainedSkill: trainedSkill))
    of turret:
      var gunIndex = -1
      for modData in module.findAll("data"):
        if dataIndex == 1:
          gunIndex = modData.attr("value").parseInt - 1
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: turret, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          gunIndex: gunIndex))
    of gun:
      var
        damage = 0
        ammoIndex = -1
      for modData in module.findAll("data"):
        case dataIndex
        of 1:
          ammoIndex = modData.attr("value").parseInt - 1
        of 2:
          damage = modData.attr("value").parseInt
        else:
          discard
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: gun, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          damage: damage, ammoIndex: ammoIndex))
    of cargoRoom:
      playerShip.modules.add(ModuleData(mType: cargoRoom, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
    of hull:
      var installedModules, maxModules = 0
      for modData in module.findAll("data"):
        case dataIndex
        of 1:
          installedModules = modData.attr("value").parseInt - 1
        of 2:
          maxModules = modData.attr("value").parseInt
        else:
          discard
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: hull, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          installedModules: installedModules, maxModules: maxModules))
    of armor:
      playerShip.modules.add(ModuleData(mType: armor, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction))
    of batteringRam:
      var damage = 0
      for modData in module.findAll("data"):
        if dataIndex == 1:
          damage = modData.attr("value").parseInt
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: batteringRam, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          damage2: damage, coolingDown: false))
    of harpoonGun:
      var
        duration = 0
        harpoonIndex = -1
      for modData in module.findAll("data"):
        case dataIndex
        of 1:
          harpoonIndex = modData.attr("value").parseInt - 1
        of 2:
          duration = modData.attr("value").parseInt
        else:
          discard
        dataIndex.inc
      playerShip.modules.add(ModuleData(mType: harpoonGun, name: name,
          protoIndex: protoIndex, weight: weight, durability: modDur,
          maxDurability: maxDur, owner: owners,
          upgradeProgress: upgradeProgress, upgradeAction: upgradeAction,
          duration: duration, harpoonIndex: harpoonIndex))
  for cargo in shipNode.findAll("cargo"):
    let
      protoIndex = cargo.attr("index").parseInt
      amount = cargo.attr("amount").parseInt
      name = cargo.attr("name")
      itemDurability = cargo.attr("durability").parseInt
      price = (if cargo.attr("price").len == 0: 0 else: cargo.attr(
          "price").parseInt)
    playerShip.cargo.add(InventoryData(protoIndex: protoIndex, amount: amount,
        name: name, durability: itemDurability, price: price))
  for crew in shipNode.findAll("member"):
    var member: MemberData
    member.name = crew.attr("name")
    member.gender = crew.attr("gender")[0]
    member.health = crew.attr("health").parseInt
    member.tired = crew.attr("tired").parseInt
    member.hunger = crew.attr("hunger").parseInt
    member.thirst = crew.attr("thirst").parseInt
    member.order = crew.attr("order").parseInt.CrewOrders
    member.previousOrder = crew.attr("previousorder").parseInt.CrewOrders
    member.orderTime = crew.attr("ordertime").parseInt
    member.payment[1] = crew.attr("dailypay").parseInt
    member.payment[2] = crew.attr("tradepay").parseInt
    member.contractLength = crew.attr("contractlength").parseInt
    member.morale[1] = crew.attr("moralelevel").parseInt
    member.morale[2] = crew.attr("moralepoints").parseInt
    member.loyalty = crew.attr("loyalty").parseInt
    for skill in crew.findAll("skill"):
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
      member.inventory.add(InventoryData(protoIndex: itemIndex, amount: amount,
          name: itemName, durability: itemDurability, price: price))
    var equipmentIndex = 1
    for item in crew.findAll("equipment"):
      member.equipment[(equipmentIndex - 1).EquipmentLocations] = item.attr(
          "index").parseInt - 1
    member.homeBase = (if crew.attr("homebase").len > 0: crew.attr(
        "homebase").parseInt else: playerShip.homeBase)
    member.faction = (if crew.attr("faction").len > 0: crew.attr(
        "faction") else: skyBases[member.homeBase].owner)
    playerShip.crew.add(member)
