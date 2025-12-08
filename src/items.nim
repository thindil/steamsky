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

## Provides code related items used by the player and NPC's, like loading
## them from files, getting random item, etc.

import std/[logging, math, paths, strutils, tables, xmlparser, xmltree]
import contracts, nimalyzer
import config, crewinventory, game, log, messages, shipscargo, shipscrew, types, utils

var
  weaponsList*: seq[Positive] = @[]
    ## The list of all weapons prototypes indexes
  shieldsList*: seq[Positive] = @[]
    ## The list of all shields prototypes indexes
  headArmorsList*: seq[Positive] = @[]
    ## The list of all head armors prototypes indexes
  chestArmorsList*: seq[Positive] = @[]
    ## The list of all chest armors prototypes indexes
  armsArmorsList*: seq[Positive] = @[]
    ## The list of all arms armors prototypes indexes
  legsArmorsList*: seq[Positive] = @[]
    ## The list of all legs armors prototypes indexes
  toolsList*: seq[string] = @[]
    ## The list of all tools prototypes indexes

proc loadItems*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the items data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    ($fileName).len > 0
  ensure:
    itemsList.len > 0
    weaponsList.len > 0
    shieldsList.len > 0
    headArmorsList.len > 0
    chestArmorsList.len > 0
    armsArmorsList.len > 0
    legsArmorsList.len > 0
  body:
    let itemsXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load items data file. Reason: " &
            getCurrentExceptionMsg())
    for itemNode in itemsXml:
      if itemNode.kind != xnElement:
        continue
      let
        itemIndex: Natural = try:
            itemNode.attr(name = "index").parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't add item '" & itemNode.attr(name = "index") & "', invalid index.")
        itemAction: DataAction = try:
            parseEnum[DataAction](s = itemNode.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if itemAction in [update, remove]:
        if itemIndex > itemsList.len():
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $itemAction & " item '" & $itemIndex & "', there is no item with that index.")
      elif itemIndex < itemsList.len():
        raise newException(exceptn = DataLoadingError,
            message = "Can't add item '" & $itemIndex & "', there is an item with that index.")
      if itemAction == DataAction.remove:
        {.warning[ProveInit]: off.}
        {.warning[UnsafeDefault]: off.}
        itemsList.del(key = itemIndex)
        {.warning[ProveInit]: on.}
        {.warning[UnsafeDefault]: on.}
        logMessage(message = "Item removed: '" & $itemIndex & "'",
            messageLevel = lvlInfo)
        continue
      var item: ObjectData = if itemAction == DataAction.update:
          try:
            itemsList[itemIndex]
          except ValueError:
            ObjectData(weight: 1, reputation: -100)
        else:
          ObjectData(weight: 1, reputation: -100)
      var attribute: XmlAttribute = itemNode.attr(name = "name")
      if attribute.len() > 0:
        item.name = attribute
      attribute = itemNode.attr(name = "weight")
      if attribute.len() > 0:
        item.weight = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $itemAction & " item '" & $itemIndex & "', invalid value for item weight.")
      attribute = itemNode.attr(name = "type")
      if attribute.len() > 0:
        item.itemType = attribute
      attribute = itemNode.attr(name = "showtype")
      if attribute.len() > 0:
        item.showType = attribute
      attribute = itemNode.attr(name = "reputation")
      if attribute.len() > 0:
        item.reputation = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $itemAction & " item '" & $itemIndex & "', invalid value for item reputation.")
      else:
        item.reputation = -100
      attribute = itemNode.attr(name = "price")
      if attribute.len() > 0:
        item.price = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $itemAction & " item '" & $itemIndex & "', invalid value for item price.")
      var valueIndex: Positive = 1
      for data in itemNode.findAll(tag = "data"):
        item.value[valueIndex] = try:
            data.attr(name = "value").parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
              message = "Can't " & $itemAction & " item '" & $itemIndex &
                  "', invalid value for item data.")
        valueIndex.inc
      attribute = itemNode.child(name = "description").innerText()
      if attribute.len() > 0:
        item.description = attribute
      if itemAction == DataAction.add:
        logMessage(message = "Item added: '" & $itemIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Item updated: '" & $itemIndex & "'",
            messageLevel = lvlInfo)
      itemsList[itemIndex] = item
      if itemIndex == moneyIndex:
        moneyName = item.name
      {.ruleOff: "ifstatements".}
      if item.itemType == weaponType:
        weaponsList.add(y = itemIndex)
      elif item.itemType == shieldType:
        shieldsList.add(y = itemIndex)
      elif item.itemType == headArmor:
        headArmorsList.add(y = itemIndex)
      elif item.itemType == chestArmor:
        chestArmorsList.add(y = itemIndex)
      elif item.itemType == armsArmor:
        armsArmorsList.add(y = itemIndex)
      elif item.itemType == legsArmor:
        legsArmorsList.add(y = itemIndex)
      {.ruleOn: "ifstatements".}

proc findProtoItem*(itemType: string): Natural {.raises: [], tags: [
    ], contractual.} =
  ## Get the index of prototype of the selected item type
  ##
  ## * itemType - the type of items which prototype's index will be get
  ##
  ## Returns the index of the prototype of the selected item's type or 0 if no prototype
  ## found
  require:
    itemType.len > 0
  body:
    for index, item in itemsList:
      if item.itemType == itemType:
        return index
    return 0

func getItemDamage*(itemDurability: ItemsDurability; toLower: bool = false;
    withColors: bool = false): string {.raises: [], tags: [], contractual.} =
  ## Get the description of the item damage level
  ##
  ## * itemDurability - the durability of the item which description will be get
  ## * toLower        - if true, convert the description to lower letters
  ## * withColors     - if true, add colors' tags to the description
  ##
  ## Returns the description of the item damage level or empty string if the item isn't
  ## damaged
  let damage: float = 1.0 - (itemDurability.float / 100.0)
  result = ""
  {.ruleOff: "ifstatements".}
  if damage < 0.2:
    result = (if withColors: "{green}" else: "") & "Slightly used" & (
        if withColors: "{/green}" else: "")
  elif damage < 0.5:
    result = (if withColors: "{gold}" else: "") & "Damaged" & (
        if withColors: "{/gold}" else: "")
  elif damage < 0.8:
    result = (if withColors: "{gold}" else: "") & "Heavily damaged" & (
        if withColors: "{/gold}" else: "")
  else:
    result = (if withColors: "{red}" else: "") & "Almost destroyed" & (
        if withColors: "{/gold}" else: "")
  {.ruleOn: "ifstatements".}
  if toLower:
    result = toLowerAscii(s = result)

proc getItemName*(item: InventoryData; damageInfo: bool = true;
    toLower: bool = true): string {.raises: [], tags: [],
        contractual.} =
  ## Get the name of the selected item with optional info about the item's
  ## damage
  ##
  ## * item       - the item which the name will be get
  ## * damageInfo - if true, add information about item's damage status
  ## * toLower    - if true, the damage info should be in lower characters
  ##
  ## Returns the name of the selected item with optional info about the item's damage
  ## status
  if item.name.len > 0:
    result = item.name
  else:
    try:
      result = itemsList[item.protoIndex].name
    except KeyError:
      return ""
  if damageInfo and item.durability < 100:
    result = result & " (" & getItemDamage(itemDurability = item.durability,
        toLower = toLower) & ")"

proc getItemChanceToDamage*(itemData: Natural): string {.raises: [],
    tags: [], contractual.} =
  ## Get the string with textual information about the item's chance for take
  ## damage during usage
  ##
  ## * itemData - the numerical chance for damage for the selected item
  ##
  ## Returns the string with textual value for the selected numerical chance for damage
  ## or numerical value if the proper setting of the game is enabled
  if gameSettings.showNumbers:
    return " " & $itemData & "%"
  case itemData
  of 1:
    return "Almost never"
  of 2:
    return "Very small"
  of 3:
    return "Small"
  of 4..9:
    return "Below average"
  of 10..14:
    return "Average"
  of 15..19:
    return "High"
  else:
    return "Very high"

proc setToolsList*() {.raises: [], tags: [], contractual.} =
  ## Set the list of all available tools in the game
  ensure:
    toolsList.len > 0
  body:
    if toolsList.len() > 0:
      return
    toolsList.add(y = repairTools)
    toolsList.add(y = cleaningTools)
    toolsList.add(y = alchemyTools)
    for recipe in recipesList.values:
      if recipe.tool notin toolsList:
        toolsList.add(y = recipe.tool)
    for skill in skillsList.values:
      if skill.tool notin toolsList:
        toolsList.add(y = skill.tool)

proc findTools*(memberIndex: Natural; itemType: string; order: CrewOrders;
    toolQuality: Positive = 100): ExtendedNatural {.raises: [KeyError,
    CrewNoSpaceError, CrewOrderError, Exception], tags: [RootEffect],
    contractual.} =
  ## Search for specified tools in the crew member and the ship cargo
  ##
  ## * memberIndex - The index of the crew member which will be checked
  ## * itemType    - The type of the item which will be looking for
  ## * order       - The order which crew member will be doing when he/she find
  ##                 the proper tool
  ## * toolQuality - The minimal quality of tool to find. Default value is 100
  ##
  ## Returns the selected crew member inventory index of the tool or -1 if
  ## tool was not found
  require:
    memberIndex < playerShip.crew.len
  ensure:
    result < playerShip.crew[memberIndex].inventory.len
  body:
    result = playerShip.crew[memberIndex].equipment[tool]
    if result > -1:
      let protoIndex: Natural = playerShip.crew[memberIndex].inventory[
          result].protoIndex
      if itemsList[protoIndex].itemType != itemType or itemsList[
          protoIndex].value[1] < toolQuality:
        updateCargo(ship = playerShip, protoIndex = protoIndex, amount = 1,
            durability = playerShip.crew[memberIndex].inventory[
            result].durability, quality = playerShip.crew[
            memberIndex].inventory[result].quality)
        updateInventory(memberIndex = memberIndex, amount = -1,
            inventoryIndex = result, ship = playerShip,
            quality = playerShip.crew[
            memberIndex].inventory[result].quality)
        result = -1
    result = findItem(inventory = playerShip.crew[memberIndex].inventory,
        itemType = itemType, quality = toolQuality, itemQuality = any)
    if result == -1:
      result = findItem(inventory = playerShip.cargo, itemType = itemType,
          quality = toolQuality, itemQuality = any)
      if result > -1:
        try:
          updateInventory(memberIndex = memberIndex, amount = 1,
              protoIndex = playerShip.cargo[result].protoIndex,
              durability = playerShip.cargo[result].durability,
                  ship = playerShip, quality = playerShip.cargo[result].quality)
          updateCargo(ship = playerShip, amount = -1, cargoIndex = result,
              quality = playerShip.cargo[result].quality)
          result = findItem(inventory = playerShip.crew[memberIndex].inventory,
              itemType = itemType, quality = toolQuality, itemQuality = any)
        except CrewNoSpaceError:
          case order:
          of repair:
            addMessage(message = playerShip.crew[memberIndex].name &
                " can't continue repairs because they don't have free space in their inventory for repair tools.",
                mType = orderMessage, color = red)
          of upgrading:
            addMessage(message = playerShip.crew[memberIndex].name &
                " can't continue upgrading module because they don't have free space in their inventory for repair tools.",
                mType = orderMessage, color = red)
          of clean:
            addMessage(message = playerShip.crew[memberIndex].name &
                " can't continue cleaning ship because they don't have free space in their inventory for cleaning tools.",
                mType = orderMessage, color = red)
          of craft:
            addMessage(message = playerShip.crew[memberIndex].name &
                " can't continue manufacturing because they don't have free space in their inventory for the proper tools.",
                mType = orderMessage, color = red)
          of train:
            addMessage(message = playerShip.crew[memberIndex].name &
                " can't continue training because they don't have free space in their inventory for the proper tools.",
                mType = orderMessage, color = red)
          else:
            discard
          giveOrders(ship = playerShip, memberIndex = memberIndex,
              givenOrder = rest)
          return -1
    playerShip.crew[memberIndex].equipment[tool] = result

proc getRandomItem*(itemsIndexes: seq[Positive]; equipIndex: EquipmentLocations;
    highestLevel, weaponSkillLevel: Positive;
    factionIndex: string): Natural {.raises: [], tags: [],
        contractual.} =
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
  require:
    itemsIndexes.len > 0
    highestLevel <= SkillRange.high
    factionIndex in factionsList
  body:
    var
      itemIndex, maxIndex: Natural = 0
      newIndexes: seq[Positive] = @[]
      added: bool = false
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
      maxIndex = ((newIndexes.len - 1).float * (weaponSkillLevel.float /
          100.0) + 1.0).Positive
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

proc getQuality*(): ObjectQuality {.raises: [], tags: [], contractual.} =
  ## Get the random quality for an item
  ##
  ## Returns random quality for an item
  case getRandom(min = 1, max = 100):
    of 1:
      return poor
    of 2..5:
      return low
    of 95..99:
      return good
    of 100:
      return excellent
    else:
      return normal

proc countItemBonus*(value: int; quality: ObjectQuality): int {.raises: [],
    tags: [], contractual.} =
  ## Count a bonus from an item's quality to the selected value
  ##
  ## * value   - the base value on which the bonus will be counted
  ## * quality - the quality of an item from which the bonus will be counted
  ##
  ## Returns the value of the bonus from an item
  case quality
  of poor:
    return -((value.float * 0.2).floor.int)
  of low:
    return -((value.float * 0.1).floor.int)
  of normal, any:
    return 0
  of good:
    return (value.float * 0.1).floor.int
  of excellent:
    return (value.float * 0.2).floor.int

proc moneyAmount*(inventory: seq[InventoryData]): Natural {.raises: [], tags: [
    ], contractual.} =
  ## Get the summarized amount of money from the selected inventory
  ##
  ## * inventory - the inventory in which money will be looked for
  ##
  ## Returns the amount of money in the selected inventory
  result = 0
  for item in inventory:
    if item.protoIndex == moneyIndex:
      result += (item.amount + countItemBonus(value = item.amount,
          quality = item.quality))

proc updateMoney*(memberIndex, amount: int; quality: ObjectQuality) {.raises: [
    CrewNoSpaceError, KeyError], tags: [], contractual.} =
  ## Update the amount of money in the selected inventory
  ##
  ## * memberIndex - the index of the player's ship's crew member in which
  ##                 inventory money will be upgraded. If -1, upgrade the
  ##                 player's ship cargo instead.
  ## * amount      - the amount about which the money should be updated
  ## * quality     - the quality of money which should be updated. If any, update
  ##                 the lowest quality of money
  var inventory: seq[InventoryData] = (if memberIndex > -1: playerShip.crew[
      memberIndex].inventory else: playerShip.cargo)
  var mIndex: ExtendedNatural = -1
  if quality == any:
    {.ruleOff: "varDeclared".}
    var
      newQuality: ObjectQuality = ObjectQuality.high
      allAmount: int = amount
    while allAmount != 0:
      for index, item in inventory:
        if item.protoIndex == moneyIndex and item.quality < newQuality:
          mIndex = index
          newQuality = item.quality
      var newAmount: int = allAmount
      if newAmount < 0:
        if inventory[mIndex].amount < newAmount.abs:
          newAmount = -inventory[mIndex].amount
      allAmount -= (newAmount + countItemBonus(value = newAmount,
          quality = newQuality))
      newAmount -= countItemBonus(value = newAmount, quality = newQuality)
      if newAmount < 0:
        if inventory[mIndex].amount < newAmount.abs:
          newAmount = -inventory[mIndex].amount
      if memberIndex > -1:
        updateInventory(memberIndex = memberIndex, amount = newAmount,
            protoIndex = moneyIndex, inventoryIndex = mIndex, ship = playerShip,
            quality = newQuality)
      else:
        updateCargo(ship = playerShip, protoIndex = moneyIndex,
            amount = newAmount, cargoIndex = mIndex, quality = newQuality)
  else:
    for index, item in inventory:
      if item.protoIndex == moneyIndex and item.quality == quality:
        mIndex = index
        break
    if memberIndex > -1:
      updateInventory(memberIndex = memberIndex, amount = amount,
          protoIndex = moneyIndex, inventoryIndex = mIndex, ship = playerShip,
          quality = quality)
    else:
      updateCargo(ship = playerShip, protoIndex = moneyIndex, amount = amount,
          cargoIndex = mIndex, quality = quality)
  {.ruleOn: "varDeclared".}

