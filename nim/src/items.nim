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
import game, log

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
    protoIndex: Natural ## The index of the item's prototype
    amount: Positive ## The amount of the item in the inventory
    name: string ## The name of the item, if different than the default one
    durability: ItemsDurability ## The current durability of the item
    price: Natural ## The price for which the item was bought

const defaultItemDurability*: ItemsDurability = 100
  ## FUNCTION
  ##
  ## Default durability for the new items

var itemsList* = initTable[Positive, ObjectData]()
  ## FUNCTION
  ##
  ## The list of prototypes of all items availabla in the game

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
      logMessage(message = "Item removed: '" & $itemIndex & "'",
          debugType = everything)
    itemsList[itemIndex] = item
    if itemIndex == moneyIndex:
      moneyName = item.name

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
    toLower: bool = true): string =
  if item.name.len > 0:
    result = item.name
  else:
    result = itemsList[item.protoIndex].name
    if damageInfo and item.durability < 100:
      result = result & " (" & getItemDamage(itemDurability = item.durability,
          toLower = toLower) & ")"

# Temporary code for interfacing with Ada

type AdaObjectData* = object
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
    toLower: cint): cstring {.exportc.} =
  return getItemName(InventoryData(protoIndex: protoIndex, amount: 1,
      name: $name, durability: durability, price: 0), damageInfo == 1,
      toLower == 1).cstring
