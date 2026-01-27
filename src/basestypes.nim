# Copyright 2022-2026 Bartek thindil Jasicki
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

## Provides code related to sky bases types like loading them from file,
## getting price, checking do an item is buyable.

import std/[colors, logging, math, paths, strutils, tables, xmlparser, xmltree]
import contracts
import game, log, types

type
  PricesArray* = array[1..2, Natural]
    ## Used to set base buy and sell price for an item in the base type.
    ## 1 - sell price, 2 - buy price

  BaseTypeData* = object
    ## Used to store information about bases types
    ##
    ## * name        - the name of the base type
    ## * color       - the color used to show a base of that type on the map
    ## * trades      - the list of items available to trade in the base type
    ## * recipes     - the list of crafting recipes available on sale in the
    ##                 base type
    ## * flags       - additional flags for the base type like shipyard, barracs,
    ##                 etc.
    ## * description - the description of the base type, show in the new game screen
    name*: string
    color*: Color
    trades: Table[Positive, PricesArray]
    recipes*: seq[string]
    flags*: seq[string]
    description*: string

var basesTypesList*: Table[string, BaseTypeData] = initTable[string,
    BaseTypeData]()
  ## The list of all available bases types in the game

proc loadBasesTypes*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load available bases types from the data file
  ##
  ## * fileName - the path to the file with bases types data which will be loaded
  require:
    ($fileName).len > 0
  body:
    let basesTypesXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load bases types data file. Reason: " &
            getCurrentExceptionMsg())
    for baseTypeNode in basesTypesXml:
      if baseTypeNode.kind != xnElement:
        continue
      let
        baseTypeIndex: string = baseTypeNode.attr(name = "index")
        baseTypeAction: DataAction = try:
            parseEnum[DataAction](s = baseTypeNode.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if baseTypeAction in [update, remove]:
        if basesTypesList.hasKey(key = baseTypeIndex):
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $baseTypeAction & " base type '" &
              baseTypeIndex & "', there is no base type with that index,")
      elif basesTypesList.hasKey(key = baseTypeIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't add base type '" & baseTypeIndex & "', there is a base type with that index.")
      if baseTypeAction == DataAction.remove:
        basesTypesList.del(key = baseTypeIndex)
        logMessage(message = "Base type removed: '" & baseTypeIndex & "'",
            messageLevel = lvlInfo)
        continue
      var baseType: BaseTypeData = if baseTypeAction == DataAction.update:
          try:
            basesTypesList[baseTypeIndex]
          except KeyError:
            BaseTypeData()
        else:
          BaseTypeData()
      var attribute: string = baseTypeNode.attr(name = "name")
      if attribute.len() > 0:
        baseType.name = attribute
      attribute = baseTypeNode.attr(name = "color")
      if attribute.len() > 0:
        baseType.color = try:
            attribute.parseColor
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't add base type '" & baseTypeIndex &
                "', invalid the type's color: '" & attribute & "'.")
      for childNode in baseTypeNode:
        if childNode.kind != xnElement:
          continue
        case childNode.tag
        of "description":
          baseType.description = childNode.innerText()
        of "item":
          let
            itemIndex: int = try:
                childNode.attr(name = "index").parseInt()
              except ValueError:
                raise newException(exceptn = DataLoadingError,
                    message = "Can't " & $baseTypeAction & " base type '" &
                    baseTypeIndex & "', invalid item index '" & childNode.attr(
                    name = "index") & "'.")
            subAction: DataAction = try:
                parseEnum[DataAction](s = childNode.attr(
                    name = "action").toLowerAscii)
              except ValueError:
                DataAction.add
          if not itemsList.hasKey(key = itemIndex):
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $baseTypeAction & " base type '" &
                baseTypeIndex & "', no item with index '" & $itemIndex & "'.")
          if subAction == DataAction.add and baseType.trades.hasKey(
              key = itemIndex):
            raise newException(exceptn = DataLoadingError,
                message = "Can't add base type '" & baseTypeIndex &
                "', item with index '" & $itemIndex & "' already added.")
          if subAction == DataAction.remove:
            {.warning[ProveInit]: off.}
            {.warning[UnsafeDefault]: off.}
            baseType.trades.del(key = itemIndex)
            {.warning[UnsafeDefault]: on.}
            {.warning[ProveInit]: on.}
          else:
            let
              buyPrice: Natural = try:
                  childNode.attr(name = "buyprice").parseInt()
                except ValueError:
                  0
              sellPrice: Natural = try:
                  childNode.attr(name = "sellprice").parseInt()
                except ValueError:
                  0
            baseType.trades[itemIndex] = [1: sellPrice, 2: buyPrice]
        of "recipe":
          let
            recipeIndex: string = childNode.attr(name = "index")
            subAction: DataAction = try:
                parseEnum[DataAction](s = childNode.attr(
                    name = "action").toLowerAscii)
              except ValueError:
                DataAction.add
          if subAction == DataAction.add and recipeIndex in baseType.recipes:
            raise newException(exceptn = DataLoadingError,
                message = "Can't add base type '" & baseTypeIndex &
                "', recipe with index '" & recipeIndex & "' already added.")
          if subAction == DataAction.remove:
            for index, recipe in baseType.recipes:
              if recipe == recipeIndex:
                baseType.recipes.delete(i = index)
                break
          else:
            baseType.recipes.add(y = recipeIndex)
        of "flag":
          let
            flagName: string = childNode.attr(name = "name")
            subAction: DataAction = try:
                parseEnum[DataAction](s = childNode.attr(
                    name = "action").toLowerAscii)
              except ValueError:
                DataAction.add
          if subAction == DataAction.add and flagName in baseType.flags:
            raise newException(exceptn = DataLoadingError,
                message = "Can't add base type '" & baseTypeIndex &
                "', flag '" & flagName & "' already added.")
          if subAction == DataAction.remove:
            for index, flag in baseType.flags:
              if flag == flagName:
                baseType.flags.delete(i = index)
                break
          else:
            baseType.flags.add(y = flagName)
      if baseTypeAction == DataAction.add:
        logMessage(message = "Base type added: '" & baseTypeIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Base type updated: '" & baseTypeIndex & "'",
            messageLevel = lvlInfo)
      basesTypesList[baseTypeIndex] = baseType

proc getPrice*(baseType: string; itemIndex: Positive;
    quality: ObjectQuality): Natural {.raises: [KeyError], tags: [],
    contractual.} =
  ## Get the price of the selected item in the selected type of bases
  ##
  ## * baseType  - the type of base from which the price will be taken
  ## * itemIndex - the index of the item's prototype which price will be taken
  ## * quality   - the quality of the item which price will be taken
  ##
  ## Returns the price of the selected item
  require:
    basesTypesList.hasKey(key = baseType)
  body:
    if itemsList[itemIndex].price == 0:
      return 0

    proc countPrice(price: Natural): Natural {.raises: [], tags: [],
        contractual.} =
      ## Count price for the item, based on its quality
      case quality
      of poor:
        result = (price.float * 0.5).ceil.Natural
      of low:
        result = (price.float * 0.75).ceil.Natural
      of normal, any:
        result = price
      of good:
        result = (price.float * 1.5).floor.Natural
      of excellent:
        result = (price.float * 1.75).floor.Natural
      if result == 0:
        result = 1

    if basesTypesList[baseType].trades.hasKey(key = itemIndex):
      if basesTypesList[baseType].trades[itemIndex][1] > 0:
        return countPrice(price = basesTypesList[baseType].trades[itemIndex][1])
      elif basesTypesList[baseType].trades[itemIndex][2] > 0:
        return countPrice(price = basesTypesList[baseType].trades[itemIndex][2])
    return countPrice(price = itemsList[itemIndex].price)

proc isBuyable*(baseType: string; itemIndex: Positive; checkFlag: bool = true;
    baseIndex: ExtendedBasesRange = 0): bool {.raises: [KeyError],
    tags: [], contractual.} =
  ## Check if the selected item is buyable in the selected bases type
  ##
  ## * baseType  - the type of base in which the item will be check
  ## * itemIndex - the index of the item's prototype which will be check
  ## * checkFlag - if true, check if the base type is black market. Can be
  ##               empty. Default value is true
  ## * baseIndex - if greater than 0, check the player reputation in the
  ##               selected base. Can be empty. Default value is 0.
  ##
  ## Returns true if the item is buyable in the selected bases type, otherwise
  ## false.
  require:
    basesTypesList.hasKey(key = baseType)
  body:
    if baseIndex > 0 and skyBases[baseIndex].reputation.level < itemsList[
        itemIndex].reputation:
      return false
    if checkFlag and "blackmarket" in basesTypesList[baseType].flags and
        getPrice(baseType = baseType, itemIndex = itemIndex, quality = normal) > 0:
      return true
    if not basesTypesList[baseType].trades.hasKey(key = itemIndex):
      return false
    if basesTypesList[baseType].trades[itemIndex][1] == 0:
      return false
    return true
