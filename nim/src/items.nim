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

import std/[strutils, xmlparser, xmltree]
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

  DataLoadingError = object of CatchableError
    ## FUNCTION
    ##
    ## Used to mark problems during loading the items from files

const defaultItemDurability*: ItemsDurability = 100
  ## FUNCTION
  ##
  ## Default durability for the new items

var itemsList*: seq[ObjectData]
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
          itemNode.attr(name = "index").parseInt() - 1
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
      {.warning[UnsafeSetLen]: off.}
      itemsList.del(i = itemIndex)
      {.warning[UnsafeSetLen]: on.}
      logMessage(message = "Item removed: '" & $itemIndex & "'",
          debugType = everything)
      continue
    var item: ObjectData = if itemAction == DataAction.update:
        itemsList[itemIndex]
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
      itemsList.add(y = item)
    else:
      itemsList[itemIndex] = item
    if itemIndex == moneyIndex - 1:
      moneyName = item.name

# Temporary code for interfacing with Ada

type AdaObjectData* = object
  ## FUNCTION
  ##
  ## Used to store data for items when interfacing with Ada
  name: cstring
  weight: cint
  itemType: cstring
  price: cint
  value: array[5, cint]
  showType: cstring
  description: cstring
  reputation: cint

proc loadAdaItems(fileName: cstring; money: cint): cstring {.sideEffect,
    raises: [DataLoadingError], tags: [WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  ## FUNCTION
  ##
  ## Load items data, started from the Ada code
  ##
  ## PARAMETERS
  ##
  ## * fileName - the name of the file with items data to load
  ## * money    - the index of the item used as money
  ##
  ## RETURNS
  ##
  ## The name of the item used as money
  moneyIndex = money.Positive
  loadItems(fileName = $fileName)
  return moneyName.cstring

proc getAdaItem(index: cint; adaItem: var AdaObjectData) {.sideEffect, raises: [
    ], tags: [], exportc.} =
  ## FUNCTION
  ##
  ## Get the item data from Nim code to Ada
  ##
  ## PARAMETERS
  ##
  ## * index   - the index of the item to get
  ## * adaItem - the converted item data to Ada record
  ##
  ## RETURNS
  ##
  ## Updated adaItem parameter with the data about the selected item
  var values: array[5, cint]
  adaItem = AdaObjectData(name: "".cstring, weight: 0, itemType: "".cstring,
      price: 0, value: values, showType: "".cstring, description: "".cstring,
      reputation: -100)
  if index >= itemsList.len():
    return
  let item = itemsList[index]
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
