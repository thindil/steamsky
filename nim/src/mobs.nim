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

import std/[strutils, tables, xmlparser, xmltree]
import game, items, log, types, utils

type
  MobInventoryRecord = object
    ## Used to store data about the inventory of the mob's prototype
    protoIndex: Natural ## The index of the item's prototype
    minAmount: Natural  ## The minimal amount of the item
    maxAmount: Natural  ## The maximum amount of the item

  ProtoMobRecord = object
    ## Used to store data about mobs prototypes
    attributes: seq[MobAttributeRecord]               ## The mob's attributes
    skills: seq[SkillInfo]                            ## The mob's skills
    order: CrewOrders                                 ## The current order of the mob
    priorities: array[1..12, Natural]                 ## The orders priorities of the mob
    inventory: seq[MobInventoryRecord]                ## The inventory of the mob
    equipment: array[EquipmentLocations, int]         ## The equipment of the mob

var protoMobsList* = initTable[Positive, ProtoMobRecord]() ## The list of prototypes of all mobs availabla in the game

proc loadMobs*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## Load the Mobs data from the file
  ##
  ## * fileName - the name of the file to load
  const
    orderNames = ["Piloting", "Engineering", "Operating guns",
      "Repair ship", "Manufacturing", "Upgrading ship", "Talking in bases",
      "Healing wounded", "Cleaning ship", "Defend ship", "Board enemy ship"]
    equipmentNames = ["Weapon", "Shield", "Head", "Torso", "Arms", "Legs", "Tool"]
  let mobsXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load mobs data file. Reason: " &
          getCurrentExceptionMsg())
  for mobNode in mobsXml:
    if mobNode.kind != xnElement:
      continue
    let
      mobIndex: Natural = try:
          mobNode.attr(name = "index").parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't add mob '" & mobNode.attr(name = "index") & "', invalid index.")
      mobAction: DataAction = try:
          parseEnum[DataAction](mobNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if mobAction in [update, remove]:
      if mobIndex > protoMobsList.len():
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $mobAction & " mob '" & $mobIndex & "', there is no mob with that index.")
    elif mobIndex < protoMobsList.len():
      raise newException(exceptn = DataLoadingError,
          message = "Can't add mob '" & $mobIndex & "', there is an mob with that index.")
    if mobAction == DataAction.remove:
      {.warning[ProveInit]: off.}
      {.warning[UnsafeDefault]: off.}
      protoMobsList.del(key = mobIndex)
      {.warning[ProveInit]: on.}
      {.warning[UnsafeDefault]: on.}
      logMessage(message = "Mob removed: '" & $mobIndex & "'",
          debugType = everything)
      continue
    var mob: ProtoMobRecord = if mobAction == DataAction.update:
        try:
          protoMobsList[mobIndex]
        except ValueError:
          ProtoMobRecord()
      else:
        ProtoMobRecord()
    for skill in mobNode.findAll(tag = "skill"):
      let skillName = skill.attr(name = "name")
      var skillIndex = if skillName == "WeaponSkill":
          Natural.high
        else:
          findSkillIndex(skillName = skillName)
      if skillIndex == 0:
        raise newException(exceptn = DataLoadingError, message = "Can't " &
            $mobAction & " mob '" & $mobIndex & "', there no skill named '" &
            skillName & "'.")
      let skillAction: DataAction = try:
          parseEnum[DataAction](skill.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
      var skillLevel, minLevel, maxLevel = 0
      if skillAction in [DataAction.add, DataAction.update]:
        skillLevel = try:
          skill.attr(name = "level").parseInt()
        except ValueError:
          0
        if skillLevel == 0:
          minLevel = try:
            skill.attr(name = "minlevel").parseInt()
          except ValueError:
            0
          maxLevel = try:
            skill.attr(name = "maxlevel").parseInt()
          except ValueError:
            0
          if minLevel >= maxLevel:
            raise newException(exceptn = DataLoadingError, message = "Can't " &
                $mobAction & " mob '" & $mobIndex &
                "', invalid range for skill '" & skillName & "'.")
      case skillAction
      of DataAction.add:
        if skillLevel > 0:
          mob.skills.add(y = SkillInfo(index: skillIndex, level: skillLevel,
              experience: 0))
        else:
          mob.skills.add(y = SkillInfo(index: skillIndex, level: minLevel,
              experience: maxLevel))
      of DataAction.update:
        for mskill in mob.skills.mitems:
          if mskill.index == skillIndex:
            if skillLevel > 0:
              mskill.level = skillLevel
            else:
              mskill.level = minLevel
              mskill.experience = maxLevel
            break
      of DataAction.remove:
        mob.skills.delete(i = skillIndex)
    let attributes = mobNode.findAll(tag = "attribute")
    for i in attributes.low..attributes.high:
      let attrLevel = try:
          attributes[i].attr(name = "level").parseInt()
        except ValueError:
          0
      if attrLevel > 0:
        if mobAction == DataAction.add:
          mob.attributes.add(y = MobAttributeRecord(level: attrLevel,
              experience: 0))
        else:
          mob.attributes[i] = MobAttributeRecord(level: attrLevel, experience: 0)
      else:
        let minLevel = try:
          attributes[i].attr(name = "minlevel").parseInt()
        except ValueError:
          0
        let maxLevel = try:
          attributes[i].attr(name = "maxlevel").parseInt()
        except ValueError:
          0
        if minLevel >= maxLevel:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $mobAction & " mob '" & $mobIndex & "', invalid range for attribute.")
        if mobAction == DataAction.add:
          mob.attributes.add(y = MobAttributeRecord(level: minLevel,
              experience: maxLevel))
        else:
          mob.attributes[i] = MobAttributeRecord(level: minLevel,
              experience: maxLevel)
    for priority in mobNode.findAll(tag = "priority"):
      for index, order in orderNames.pairs:
        if order == priority.attr(name = "name"):
          mob.priorities[index] = if priority.attr(name = "value") == "Normal":
              1
            else:
              2
          break
    var mobOrder = mobNode.attr(name = "order")
    if mobOrder.len > 0:
      mob.order = try:
          parseEnum[CrewOrders](mobOrder.toLowerAscii)
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $mobAction & " mob '" &
                  $mobIndex & "', invalid order for the mob.")
    for item in mobNode.findAll(tag = "item"):
      let itemIndex = try:
            item.attr(name = "index").parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $mobAction & " mob '" &
                    $mobIndex & "', invalid index of item.")
      if itemIndex > itemsList.len:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $mobAction & " mob '" &
                $mobIndex & "', there is no item with index '" & $itemIndex & "'.")
      let itemAction: DataAction = try:
          parseEnum[DataAction](item.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
      var amount, minAmount, maxAmount = 0
      if itemAction in [DataAction.add, DataAction.update]:
        amount = try:
            item.attr(name = "amount").parseInt()
          except ValueError:
            0
        if amount == 0:
          minAmount = try:
            item.attr(name = "minamount").parseInt()
          except ValueError:
            0
          maxAmount = try:
            item.attr(name = "maxamount").parseInt()
          except ValueError:
            0
          if minAmount >= maxAmount:
            raise newException(exceptn = DataLoadingError, message = "Can't " &
                $mobAction & " mob '" & $mobIndex &
                "', invalid range for item amount '" & $itemIndex & "'.")
      case itemAction
      of DataAction.add:
        if amount > 0:
          mob.inventory.add(y = MobInventoryRecord(protoIndex: itemIndex,
              minAmount: amount, maxAmount: 0))
        else:
          mob.inventory.add(y = MobInventoryRecord(protoIndex: itemIndex,
              minAmount: minAmount, maxAmount: maxAmount))
      of DataAction.update:
        for mitem in mob.inventory.mitems:
          if mitem.protoIndex == itemIndex:
            if amount > 0:
              mitem = MobInventoryRecord(protoIndex: itemIndex,
                  minAmount: amount, maxAmount: 0)
            else:
              mitem = MobInventoryRecord(protoIndex: itemIndex,
                  minAmount: minAmount, maxAmount: maxAmount)
            break
      of DataAction.remove:
        var deleteIndex = -1
        for index, mitem in mob.inventory.pairs:
          if mitem.protoIndex == itemIndex:
            deleteIndex = index
            break
        if deleteIndex > -1:
          mob.inventory.delete(i = deleteIndex)
    for item in mobNode.findAll(tag = "equipment"):
      let slotName = item.attr(name = "slot")
      for index, name in equipmentNames.pairs:
        if name == slotName:
          mob.equipment[index.EquipmentLocations] = try:
              item.attr(name = "index").parseInt()
            except ValueError:
              raise newException(exceptn = DataLoadingError,
                  message = "Can't " & $mobAction & " mob '" & $mobIndex &
                  "', invalid equipment index '" & item.attr(name = "index") & "'.")
          break
    if mobAction == DataAction.add:
      logMessage(message = "Mob added: '" & $mobIndex & "'",
          debugType = everything)
    else:
      logMessage(message = "Mob updated: '" & $mobIndex & "'",
          debugType = everything)
    protoMobsList[mobIndex] = mob

proc getRandomItem*(itemsIndexes: seq[Positive], equipIndex: EquipmentLocations,
    highestLevel, weaponSkillLevel: Positive,
    factionIndex: string): Natural {.sideEffect, raises: [], tags: [].} =
  ## Get the random index of the item of the selected type
  ##
  ## * itemsIndexes     - the list of indexes of the items from which the index will be get
  ## * equipIndex       - the position of the item in equipment (like weapon, tools, etc)
  ## * highestLevel     - the highest level of skills for the selected mob
  ## * weaponSkillLevel - the index of the weapon skill for the selected mob
  ## * factionIndex     - the index of the faction to which the mob belongs
  ##
  ## Returns the random index from the selected itemsIndexes list of 0 if the item
  ## can't be get
  var
    itemIndex, maxIndex: Natural
    newIndexes: seq[Positive]
    added: bool
  if equipIndex > weapon:
    try:
      for index in itemsIndexes:
        added = false
        for j in 0..<newIndexes.len:
          if itemsList[index].price < itemsList[newIndexes[j]].price:
            {.warning[UnsafeSetLen]: off.}
            newIndexes.insert(item = index, i = j)
            {.warning[UnsafeSetLen]: on.}
            added = true
            break
        if not added:
          newIndexes.add(y = index)
    except KeyError:
      return 0
    maxIndex = ((newIndexes.len - 1).float * (highestLevel.float / 100.0) + 1.0).Positive
    if maxIndex > newIndexes.len - 1:
      maxIndex = newIndexes.len - 1
    itemIndex = getRandom(min = 0, max = maxIndex)
  else:
    try:
      for index in itemsIndexes:
        added = false
        for j in 0..<newIndexes.len:
          if itemsList[index].price < itemsList[newIndexes[j]].price and
              itemsList[index].value[3] == factionsList[
                  factionIndex].weaponSkill:
            {.warning[UnsafeSetLen]: off.}
            newIndexes.insert(item = index, i = j)
            {.warning[UnsafeSetLen]: on.}
            added = true
            break
        if not added and itemsList[index].value[3] == factionsList[
            factionIndex].weaponSkill:
          newIndexes.add(y = index)
    except KeyError:
      return 0
    if newIndexes.len == 0:
      return 0
    maxIndex = ((newIndexes.len - 1).float * (weaponSkillLevel.float / 100.0) + 1.0).Positive
    if maxIndex > newIndexes.len - 1:
      maxIndex = newIndexes.len - 1
    try:
      while true:
        itemIndex = getRandom(min = 0, max = maxIndex)
        if itemsList[newIndexes[itemIndex]].value[3] == factionsList[
            factionIndex].weaponSkill:
          break
    except KeyError:
      return 0
  for index in itemsIndexes:
    if index == newIndexes[itemIndex]:
      return newIndexes[itemIndex]
  return 0

# Temporary code for interfacing with Ada

proc getAdaRandomItem(items: cstring, equipIndex, highestLevel,
    weaponSkillLevel: cint; factionIndex: cstring;
        highestSkill: cint): cint {.sideEffect, raises: [], tags: [], exportc.} =
  case $items
  of "weapon":
    return getRandomItem(itemsIndexes = weaponsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "shield":
    return getRandomItem(itemsIndexes = shieldsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "helmet":
    return getRandomItem(itemsIndexes = headArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "torso":
    return getRandomItem(itemsIndexes = chestArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "arms":
    return getRandomItem(itemsIndexes = armsArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "legs":
    return getRandomItem(itemsIndexes = legsArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "tool":
    var tempToolsList: seq[Positive]
    for recipe in recipesList.values:
      if highestSkill == recipe.skill:
        for index, item in itemsList.pairs:
          if item.itemType == recipe.tool:
            tempToolsList.add(y = index)
        break
    if tempToolsList.len == 0:
      return 0
    return getRandomItem(itemsIndexes = tempToolsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
