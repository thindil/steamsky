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

{.used.}

import std/[strutils, tables, xmlparser, xmltree]
import config, crafts, game, log

type
  ObjectData* = object
    ## FUNCTION
    ##
    ## Used to store information about items
    name: string ## The name of the item
    weight: Positive ## The weight of the item
    itemType: string ## The type of the item
    price: Natural ## The base price of the item in bases
    value: seq[int] ## Various data related to the item (damage for ammo, etc.)
    showType: string ## The item's type to show to the player instead of the itemType
    description: string ## The description of the item
    reputation: ReputationRange ## The minumal reputation which is needed to buy that item

  ItemsDurability* = range[0..101]
  ## FUNCTION
  ##
  ## Used to set items durability

  InventoryData* = object
    ## FUNCTION
    ##
    ## Used to store information about items in various inventories (cargo, crew
    ## inventory, ect)
    protoIndex*: Natural ## The index of the item's prototype
    amount*: Positive ## The amount of the item in the inventory
    name*: string ## The name of the item, if different than the default one
    durability*: ItemsDurability ## The current durability of the item
    price*: Natural ## The price for which the item was bought

const defaultItemDurability*: ItemsDurability = 100
  ## FUNCTION
  ##
  ## Default durability for the new items

var
  itemsList* = initTable[Positive, ObjectData]()
    ## FUNCTION
    ##
    ## The list of prototypes of all items availabla in the game
  weaponsList*: seq[Positive]
    ## FUNCTION
    ##
    ## The list of all weapons prototypes indexes
  shieldsList*: seq[Positive]
    ## FUNCTION
    ##
    ## The list of all shields prototypes indexes
  headArmorsList*: seq[Positive]
    ## FUNCTION
    ##
    ## The list of all head armors prototypes indexes
  chestArmorsList*: seq[Positive]
    ## FUNCTION
    ##
    ## The list of all chest armors prototypes indexes
  armsArmorsList*: seq[Positive]
    ## FUNCTION
    ##
    ## The list of all arms armors prototypes indexes
  legsArmorsList*: seq[Positive]
    ## FUNCTION
    ##
    ## The list of all legs armors prototypes indexes
  toolsList*: seq[string]

proc loadItems*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## FUNCTION
  ##
  ## Load the items data from the file
  ##
  ## PARAMETERS
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
    for data in itemNode.findAll(tag = "data"):
      item.value.add(y = try:
          data.attr(name = "value").parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $itemAction & " item '" & $itemIndex &
                "', invalid value for item data."))
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
  ## FUNCTION
  ##
  ## Get the index of prototype of the selected item type
  ##
  ## PARAMETERS
  ##
  ## * itemType - the type of items which prototype's index will be get
  ##
  ## RETURNS
  ##
  ## The index of the prototype of the selected item's type or 0 if no prototype
  ## found
  for index, item in itemsList.pairs():
    if item.itemType == itemType:
      return index
  return 0

func getItemDamage*(itemDurability: ItemsDurability;
    toLower: bool = false): string {.raises: [], tags: [].} =
  ## FUNCTION
  ##
  ## Get the description of the item damage level
  ##
  ## PARAMETERS
  ##
  ## * itemDurability - the durability of the item which description will be get
  ## * toLower        - if true, convert the description to lower letters
  ##
  ## RETURNS
  ##
  ## The description of the item damage level or empty string if the item isn't
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
  ## FUNCTION
  ##
  ## Get the name of the selected item with optional info about the item's
  ## damage
  ##
  ## PARAMETERS
  ##
  ## * item       - the item which the name will be get
  ## * damageInfo - if true, add information about item's damage status
  ## * toLower    - if true, the damage info should be in lower characters
  ##
  ## RETURNS
  ##
  ## The name of the selected item with optional info about the item's damage
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
  ## FUNCTION
  ##
  ## Get the string with textual information about the item's chance for take
  ## damage during usage
  ##
  ## PARAMETERS
  ##
  ## * itemData - the numerical chance for damage for the selected item
  ##
  ## RETURNS
  ##
  ## The string with textual value for the selected numerical chance for damage
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
  ## FUNCTION
  ##
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

proc findItem*(inventory: Table[Positive, InventoryData];
    protoIndex: Natural = 0; itemType: string = "";
    durability: ItemsDurability = ItemsDurability.high;
    quality: Positive = 100): Natural {.sideEffect, raises: [], tags: [].} =
  ## FUNCTION
  ##
  ## Find the index of the selected item in the selected inventory
  ##
  ## PARAMETERS
  ##
  ## * protoIndex - the index of prototype item of the item to find. Can be
  ##                empty. If empty, itemType parameter must be set
  ## * itemType   - the type of prototype item of the item to find. Can be
  ##                empty. If empty, protoIndex parameter must be set
  ## * durability - the durability of the item to find. Can be empty
  ## * quality    - the quality of the item to find. Can be empty
  ##
  ## RETURNS
  ##
  ## The index of the item in the selected inventory which meet searching
  ## criteria or 0 if item not found.
  try:
    if protoIndex > 0:
      for index, item in inventory.pairs:
        if item.protoIndex == protoIndex and itemsList[protoIndex].value[0] <= quality:
          if durability < ItemsDurability.high and item.durability == durability:
            return index
          else:
            return index
    elif itemType.len > 0:
      for index, item in inventory.pairs:
        if itemsList[item.protoIndex].itemType == itemType and itemsList[
            item.protoIndex].value[0] <= quality:
          if durability < ItemsDurability.high and item.durability == durability:
            return index
          else:
            return index
  except KeyError:
    discard
  return 0

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

  AdaInventoryData = object
    protoIndex: cint
    amount: cint
    name: cstring
    durability: cint
    price: cint

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
    values[index] = item.cint
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

proc getAdaItemsList(name: cstring; itemsList: var array[64,
    cint]) {.sideEffect, raises: [], tags: [], exportc.} =
  for i in 0..63:
    itemsList[i] = 0
  if name == "weapons":
    for index, item in weaponsList.pairs:
      itemsList[index] = item.cint
  elif name == "shields":
    for index, item in shieldsList.pairs:
      itemsList[index] = item.cint
  elif name == "headarmors":
    for index, item in headArmorsList.pairs:
      itemsList[index] = item.cint
  elif name == "chestarmors":
    for index, item in chestArmorsList.pairs:
      itemsList[index] = item.cint
  elif name == "legsarmors":
    for index, item in legsArmorsList.pairs:
      itemsList[index] = item.cint
  elif name == "armsarmors":
    for index, item in armsArmorsList.pairs:
      itemsList[index] = item.cint

proc getAdaItemChanceToDamage(itemData: cint): cstring {.sideEffect, raises: [
    ], tags: [], exportc.} =
  return getItemChanceToDamage(itemData).cstring

proc setAdaToolsList() {.sideEffect, raises: [], tags: [], exportc.} =
  setToolsList()

proc findAdaItem(inventory: array[128, AdaInventoryData]; protoIndex: cint;
    itemType: cstring; durability: cint; quality: cint): cint {.sideEffect,
    raises: [], tags: [], exportc.} =
  var newInventory = initTable[Positive, InventoryData]()
  for i in 0..127:
    if inventory[i].protoIndex == 0:
      break
    newInventory[i + 1] = InventoryData(protoIndex: inventory[i].protoIndex,
        amount: inventory[i].amount, name: $inventory[i].name,
        durability: inventory[i].durability, price: inventory[i].price)
  return findItem(inventory = newInventory, protoIndex = protoIndex,
      itemType = $itemType, durability = durability, quality = quality).cint

proc isAdaTool(itemType: cstring): cint {.sideEffect, raises: [], tags: [], exportc.} =
  if $itemType in toolsList:
    return 1
  return 0
