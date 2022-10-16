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
    name: string
    weight: Positive
    itemType: string
    price: Natural
    value: seq[int]
    showType: string
    description: string
    reputation: ReputationRange

  ItemsDurability* = range[0..101]

  InventoryData* = object
    protoIndex: Natural
    amount: Positive
    name: string
    durability: ItemsDurability
    price: Natural

  DataLoadingError = object of CatchableError

const defaultItemDurability*: ItemsDurability = 100

var itemsList*: seq[ObjectData]

proc loadItems*(fileName: string) =
  let itemsXml = loadXml(path = fileName)
  for itemNode in itemsXml:
    if itemNode.kind != xnElement:
      continue
    let
      itemIndex: Natural = try:
          itemNode.attr(name = "index").parseInt() - 1
        except ValueError:
          raise newException(exceptn = DataLoadingError,  message = "Can't add item '" & itemNode.attr(name = "index") & "', invalid index.")
      itemAction: DataAction = try:
          parseEnum[DataAction](itemNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if itemAction in [update, remove]:
      if itemIndex > itemsList.len():
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $itemAction & " faction '" & $itemIndex & "', there is no item with that index,")
    elif itemIndex < itemsList.len():
        raise newException(exceptn = DataLoadingError,
            message = "Can't add item '" & $itemIndex & "', there is an item with that index.")
    if itemAction == DataAction.remove:
      {.warning[UnsafeSetLen]: off.}
      itemsList.del(i = itemIndex)
      {.warning[UnsafeSetLen]: on.}
      logMessage(message = "Item removed: '" & $itemIndex & "'", debugType = everything)
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
      item.weight = attribute.parseInt()
    attribute = itemNode.attr(name = "type")
    if attribute.len() > 0:
      item.itemType = attribute
    attribute = itemNode.attr(name = "showtype")
    if attribute.len() > 0:
      item.showType = attribute
    attribute = itemNode.attr(name = "reputation")
    if attribute.len() > 0:
      item.reputation = attribute.parseInt()
    attribute = itemNode.attr(name = "price")
    if attribute.len() > 0:
      item.price = attribute.parseInt()
    for data in itemNode.findAll(tag = "data"):
      item.value.add(y = data.attr(name = "value").parseInt())
    attribute = itemNode.child(name = "description").innerText()
    if attribute.len() > 0:
      item.description = attribute
    if itemAction == DataAction.add:
      itemsList.add(y = item)
    else:
      itemsList[itemIndex] = item
    if itemIndex == moneyIndex - 1:
      moneyName = item.name

proc loadAdaItems*(fileName: cstring) {.exportc.} =
  loadItems(fileName = $fileName)
