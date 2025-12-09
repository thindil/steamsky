# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to the game's mobs, like loading them from files and
## generating random one.

import std/[logging, paths, strutils, tables, xmlparser, xmltree]
import contracts
import factions, game, items, log, shipscrew, types, utils

proc loadSkills(mobNode: XmlNode; mob: var ProtoMobRecord;
    mobAction: DataAction; mobIndex: Natural) {.raises: [DataLoadingError],
        tags: [], contractual.} =
  ## Load skills of the selected mobile's prototype
  ##
  ## * mobNode   - the XML node with data about the mobile's prototype
  ## * mob       - the mobile prototype to which the skills will be modified
  ## * mobAction - the action to take with skills
  ## * mobIndex  - the index of the mobile to add
  ##
  ## Returns modified parameter mob
  body:
    for skill in mobNode.findAll(tag = "skill"):
      let skillName: string = skill.attr(name = "name")
      var skillIndex: int = if skillName == "WeaponSkill":
          skillsList.len + 1
        else:
          findSkillIndex(skillName = skillName)
      if skillIndex == 0:
        raise newException(exceptn = DataLoadingError, message = "Can't " &
            $mobAction & " mob '" & $mobIndex & "', there no skill named '" &
            skillName & "'.")
      let skillAction: DataAction = try:
          parseEnum[DataAction](s = skill.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
      var skillLevel, minLevel, maxLevel: Natural = 0
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
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $mobAction & " mob '" & $mobIndex &
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

proc loadMobs*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the Mobs data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    ($fileName).len > 0
  body:
    const
      orderNames: array[11, string] = ["Piloting", "Engineering",
        "Operating guns",
        "Repair ship", "Manufacturing", "Upgrading ship", "Talking in bases",
        "Healing wounded", "Cleaning ship", "Defend ship", "Board enemy ship"]
      equipmentNames: array[7, string] = ["Weapon", "Shield", "Head", "Torso",
          "Arms", "Legs", "Tool"]
    let mobsXml: XmlNode = try:
        loadXml(path = $fileName)
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
            parseEnum[DataAction](s = mobNode.attr(
                name = "action").toLowerAscii)
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
            messageLevel = lvlInfo)
        continue
      var mob: ProtoMobRecord = if mobAction == DataAction.update:
          try:
            protoMobsList[mobIndex]
          except ValueError:
            ProtoMobRecord()
        else:
          ProtoMobRecord()
      loadSkills(mobNode = mobNode, mob = mob, mobAction = mobAction,
          mobIndex = mobIndex)
      let attributes: seq[XmlNode] = mobNode.findAll(tag = "attribute")
      for i in attributes.low..attributes.high:
        let attrLevel: Natural = try:
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
          let minLevel: Natural = try:
            attributes[i].attr(name = "minlevel").parseInt()
          except ValueError:
            0
          let maxLevel: Natural = try:
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
        for index, order in orderNames:
          if order == priority.attr(name = "name"):
            mob.priorities[index + 1] = if priority.attr(name = "value") == "Normal":
                1
              else:
                2
            break
      var mobOrder: string = mobNode.attr(name = "order")
      if mobOrder.len > 0:
        mob.order = try:
            parseEnum[CrewOrders](s = mobOrder.toLowerAscii)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $mobAction & " mob '" &
                    $mobIndex & "', invalid order for the mob.")
      for item in mobNode.findAll(tag = "item"):
        let itemIndex: int = try:
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
            parseEnum[DataAction](s = item.attr(name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
        var amount, minAmount, maxAmount: Natural = 0
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
              raise newException(exceptn = DataLoadingError,
                  message = "Can't " & $mobAction & " mob '" & $mobIndex &
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
          var deleteIndex: int = -1
          for index, mitem in mob.inventory:
            if mitem.protoIndex == itemIndex:
              deleteIndex = index
              break
          if deleteIndex > -1:
            mob.inventory.delete(i = deleteIndex)
      for item in mob.equipment.mitems:
        item = -1
      for item in mobNode.findAll(tag = "equipment"):
        let slotName: string = item.attr(name = "slot")
        for index, name in equipmentNames:
          if name == slotName:
            mob.equipment[index.EquipmentLocations] = try:
                item.attr(name = "index").parseInt() - 1
              except ValueError:
                raise newException(exceptn = DataLoadingError,
                    message = "Can't " & $mobAction & " mob '" & $mobIndex &
                    "', invalid equipment index '" & item.attr(name = "index") & "'.")
            break
      if mobAction == DataAction.add:
        logMessage(message = "Mob added: '" & $mobIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Mob updated: '" & $mobIndex & "'",
            messageLevel = lvlInfo)
      protoMobsList[mobIndex] = mob

proc generateMob*(mobIndex: Natural; factionIndex: string): MemberData {.raises: [
    KeyError], tags: [], contractual.} =
  ## Generate random mob from the selected prototype and the faction.
  ##
  ## * mobIndex     - the index of the prototype of the mob from which the new
  ##                  will be generated
  ## * factionIndex - the index of the faction to which the mob will be belong
  ##
  ## Returns the newly created mob from the selected prototype and faction.
  require:
    protoMobsList.hasKey(key = mobIndex)
    factionsList.hasKey(key = factionIndex)
  body:
    result = MemberData(homeBase: 1)
    result.faction = (if getRandom(min = 1, max = 100) <
        99: factionIndex else: getRandomFaction())
    result.gender = 'M'
    let faction: FactionData = factionsList[result.faction]
    if "nogender" notin faction.flags and getRandom(min = 1,
        max = 100) > 50:
      result.gender = 'F'
    result.name = generateMemberName(gender = result.gender,
        factionIndex = result.faction)
    var weaponSkillLevel, highestSkillLevel: Natural = 1
    let protoMob: ProtoMobRecord = protoMobsList[mobIndex]
    for skill in protoMob.skills:
      let skillIndex: int = (if skill.index >
          skillsList.len: faction.weaponSkill else: skill.index)
      if skill.experience == 0:
        result.skills.add(y = SkillInfo(index: skillIndex, level: skill.level,
            experience: 0))
      else:
        result.skills.add(y = SkillInfo(index: skillIndex, level: getRandom(
            min = skill.level, max = skill.experience), experience: 0))
      if skillIndex == faction.weaponSkill:
        weaponSkillLevel = result.skills[^1].level
      if result.skills[^1].level > highestSkillLevel:
        highestSkillLevel = result.skills[^1].level
    for attribute in protoMob.attributes:
      if attribute.experience == 0:
        result.attributes.add(y = attribute)
      else:
        result.attributes.add(y = MobAttributeRecord(level: getRandom(
            min = attribute.level, max = attribute.experience), experience: 0))
    for item in protoMob.inventory:
      let amount: int = if item.maxAmount > 0:
          getRandom(min = item.minAmount, max = item.maxAmount)
        else:
          item.minAmount
      result.inventory.add(y = InventoryData(protoIndex: item.protoIndex,
          amount: amount, name: "", durability: defaultItemDurability, price: 0))
    result.equipment = protoMob.equipment
    for i in weapon..legs:
      if result.equipment[i] == -1:
        # Don't generate shield if the mob uses two-handed weapon
        if i == shield and result.equipment[weapon] > -1 and itemsList[
            result.inventory[result.equipment[weapon]].protoIndex].value[4] == 2:
          continue
        var equipmentItemIndex: Natural = 0
        if getRandom(min = 1, max = 100) < 95:
          let equipmentItemsList: seq[Positive] = case i
            of weapon:
              weaponsList
            of shield:
              shieldsList
            of helmet:
              headArmorsList
            of torso:
              chestArmorsList
            of arms:
              armsArmorsList
            else:
              legsArmorsList
          equipmentItemIndex = getRandomItem(itemsIndexes = equipmentItemsList,
              equipIndex = i, highestLevel = highestSkillLevel,
              weaponSkillLevel = weaponSkillLevel,
              factionIndex = result.faction)
        if equipmentItemIndex > 0:
          result.inventory.add(y = InventoryData(protoIndex: equipmentItemIndex,
              amount: 1, name: "", durability: defaultItemDurability, price: 0))
          result.equipment[i] = result.inventory.high
    result.orders = protoMob.priorities
    result.order = protoMob.order
    result.orderTime = 15
    result.previousOrder = rest
    result.health = 100
    result.tired = 0
    result.hunger = 0
    result.thirst = 0
    result.payment = [1: 20.Natural, 2: 0.Natural]
    result.contractLength = -1
    result.morale = [1: (if "fanaticism" in
        faction.flags: 100.Natural else: 50.Natural), 2: 0.Natural]
    result.loyalty = 100
    result.homeBase = 1
