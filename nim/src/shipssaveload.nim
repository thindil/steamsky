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

import std/xmltree
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
