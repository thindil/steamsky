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
import config, crewinventory, game, log, messages, ships, shipscargo, shipscrew, types

var
  weaponsList*: seq[Positive]
    ## The list of all weapons prototypes indexes
  shieldsList*: seq[Positive]
    ## The list of all shields prototypes indexes
  headArmorsList*: seq[Positive]
    ## The list of all head armors prototypes indexes
  chestArmorsList*: seq[Positive]
    ## The list of all chest armors prototypes indexes
  armsArmorsList*: seq[Positive]
    ## The list of all arms armors prototypes indexes
  legsArmorsList*: seq[Positive]
    ## The list of all legs armors prototypes indexes
  toolsList*: seq[string]
    ## The list of all tools prototypes indexes

proc loadItems*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## Load the items data from the file
  ##
  ## * fileName - the name of the file to load
  let itemsXml = try:
      loadXml(path = fileName)
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
          parseEnum[DataAction](itemNode.attr(name = "action").toLowerAscii)
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
          debugType = everything)
      continue
    var item: ObjectData = if itemAction == DataAction.update:
        try:
          itemsList[itemIndex]
        except ValueError:
          ObjectData(weight: 1, reputation: -100)
      else:
        ObjectData(weight: 1, reputation: -100)
    var attribute = itemNode.attr(name = "name")
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
          debugType = everything)
    else:
      logMessage(message = "Item updated: '" & $itemIndex & "'",
          debugType = everything)
    itemsList[itemIndex] = item
    if itemIndex == moneyIndex:
      moneyName = item.name
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

proc findProtoItem*(itemType: string): Natural {.sideEffect, raises: [], tags: [].} =
  ## Get the index of prototype of the selected item type
  ##
  ## * itemType - the type of items which prototype's index will be get
  ##
  ## Returns the index of the prototype of the selected item's type or 0 if no prototype
  ## found
  for index, item in itemsList.pairs():
    if item.itemType == itemType:
      return index
  return 0

func getItemDamage*(itemDurability: ItemsDurability;
    toLower: bool = false): string {.raises: [], tags: [].} =
  ## Get the description of the item damage level
  ##
  ## * itemDurability - the durability of the item which description will be get
  ## * toLower        - if true, convert the description to lower letters
  ##
  ## Returns the description of the item damage level or empty string if the item isn't
  ## damaged
  let damage: float = 1.0 - (itemDurability.float / 100.0)
  result = ""
  if damage < 0.2:
    result = "Slightly used"
  elif damage < 0.5:
    result = "Damaged"
  elif damage < 0.8:
    result = "Heavily damaged"
  else:
    result = "Almost destroyed"
  if toLower:
    result = toLowerAscii(s = result)

proc getItemName*(item: InventoryData; damageInfo,
    toLower: bool = true): string {.sideEffect, raises: [], tags: [].} =
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

proc getItemChanceToDamage*(itemData: Natural): string {.sideEffect, raises: [],
    tags: [].} =
  ## Get the string with textual information about the item's chance for take
  ## damage during usage
  ##
  ## * itemData - the numerical chance for damage for the selected item
  ##
  ## Returns the string with textual value for the selected numerical chance for damage
  ## or numerical value if the proper setting of the game is enabled
  if gameSettings.showNumbers == 1:
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

proc setToolsList*() {.sideEffect, raises: [], tags: [].} =
  ## Set the list of all available tools in the game
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
    toolQuality: Positive = 100): int {.sideEffect, raises: [KeyError,
    CrewNoSpaceError, CrewOrderError, Exception], tags: [RootEffect].} =
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
  result = playerShip.crew[memberIndex].equipment[tool]
  if result > -1:
    let protoIndex = playerShip.crew[memberIndex].inventory[result].protoIndex
    if itemsList[protoIndex].itemType != itemType or itemsList[
        protoIndex].value[1] < toolQuality:
      updateCargo(ship = playerShip, protoIndex = protoIndex, amount = 1,
          durability = playerShip.crew[memberIndex].inventory[
          result].durability)
      updateInventory(memberIndex = memberIndex, amount = -1,
          inventoryIndex = result, ship = playerShip)
      result = -1
  result = findItem(inventory = playerShip.crew[memberIndex].inventory,
      itemType = itemType, quality = toolQuality)
  if result == -1:
    result = findItem(inventory = playerShip.cargo, itemType = itemType,
        quality = toolQuality)
    if result > -1:
      try:
        updateInventory(memberIndex = memberIndex, amount = 1,
            protoIndex = playerShip.cargo[result].protoIndex,
            durability = playerShip.cargo[result].durability, ship = playerShip)
        updateCargo(ship = playerShip, amount = -1, cargoIndex = result)
        result = findItem(inventory = playerShip.crew[memberIndex].inventory,
            itemType = itemType, quality = toolQuality)
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

# Temporary code for interfacing with Ada

type
  AdaObjectData* = object
    name: cstring
    weight: cint
    itemType: cstring
    price: cint
    value: array[5, cint]
    showType: cstring
    description: cstring
    reputation: cint

proc loadAdaItems(fileName: cstring): cstring {.sideEffect,
    raises: [DataLoadingError], tags: [WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  loadItems(fileName = $fileName)
  return moneyName.cstring

proc getAdaItem(index: cint; adaItem: var AdaObjectData) {.sideEffect, raises: [
    ], tags: [], exportc.} =
  var values: array[5, cint]
  adaItem = AdaObjectData(name: "".cstring, weight: 0, itemType: "".cstring,
      price: 0, value: values, showType: "".cstring, description: "".cstring,
      reputation: -100)
  if not itemsList.hasKey(key = index):
    return
  let item = try:
      itemsList[index]
    except KeyError:
      return
  adaItem.name = item.name.cstring
  adaItem.weight = item.weight.cint
  adaItem.itemType = item.itemType.cstring
  adaItem.price = item.price.cint
  for index, item in item.value.pairs:
    values[index - 1] = item.cint
  adaItem.value = values
  adaItem.showType = item.showType.cstring
  adaItem.description = item.description.cstring
  adaItem.reputation = item.reputation.cint

proc findAdaProtoItem(itemType: cstring): cint {.sideEffect, raises: [], tags: [], exportc.} =
  return findProtoItem(itemType = $itemType).cint

func getAdaItemDamage(itemDurability: cint; toLower: cint): cstring {.raises: [
    ], tags: [], exportc.} =
  return getItemDamage(itemDurability.ItemsDurability, toLower == 1).cstring

proc getAdaItemName(name: cstring; protoIndex, durability, damageInfo,
    toLower: cint): cstring {.sideEffect, raises: [], tags: [], exportc.} =
  return getItemName(InventoryData(protoIndex: protoIndex, amount: 1,
      name: $name, durability: durability, price: 0), damageInfo == 1,
      toLower == 1).cstring

proc getAdaItemChanceToDamage(itemData: cint): cstring {.sideEffect, raises: [
    ], tags: [], exportc.} =
  return getItemChanceToDamage(itemData).cstring

proc setAdaToolsList() {.sideEffect, raises: [], tags: [], exportc.} =
  setToolsList()

proc isAdaTool(itemType: cstring): cint {.sideEffect, raises: [], tags: [], exportc.} =
  if $itemType in toolsList:
    return 1
  return 0

proc getAdaProtoAmount(): cint {.exportc.} =
  return itemsList.len.cint

proc findAdaTools(memberIndex: cint; itemType: cstring; order,
    toolQuality: cint): cint {.exportc.} =
  return findTools(memberIndex = (memberIndex - 1).Natural,
      itemType = $itemType,

order = order.CrewOrders, toolQuality = toolQuality.Positive).cint + 1
