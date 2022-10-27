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
import game, items, log

type
  PricesArray* = array[1..2, Natural]

  BaseTypeData* = object
    name: string
    color: string
    trades: Table[Positive, PricesArray]
    recipes: seq[string]
    flags: seq[string]
    description: string

var basesTypesList* = initTable[string, BaseTypeData]()

proc loadBasesTypes*(fileName: string) =
  let basesTypesXml = loadXml(path = fileName)
  for baseTypeNode in basesTypesXml:
    if baseTypeNode.kind != xnElement:
      continue
    let
      baseTypeIndex = baseTypeNode.attr(name = "index")
      baseTypeAction: DataAction = try:
          parseEnum[DataAction](baseTypeNode.attr(name = "action").toLowerAscii)
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
      logMessage(message = "Base yype removed: '" & baseTypeIndex & "'",
          debugType = everything)
      continue
    var baseType: BaseTypeData = if baseTypeAction == DataAction.update:
        basesTypesList[baseTypeIndex]
      else:
        BaseTypeData()
    var attribute = baseTypeNode.attr(name = "name")
    if attribute.len() > 0:
      baseType.name = attribute
    attribute = baseTypeNode.attr(name = "color")
    if attribute.len() > 0:
      baseType.color = attribute
    for childNode in baseTypeNode:
      if childNode.kind != xnElement:
        continue
      case childNode.tag
      of "description":
        baseType.description = childNode.innerText()
      of "item":
        let
          itemIndex = childNode.attr(name = "index").parseInt()
          subAction = try:
              parseEnum[DataAction](childNode.attr(
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
          recipeIndex = childNode.attr(name = "index")
          subAction = try:
              parseEnum[DataAction](childNode.attr(
                  name = "action").toLowerAscii)
            except ValueError:
              DataAction.add
        if subAction == DataAction.add and recipeIndex in baseType.recipes:
          raise newException(exceptn = DataLoadingError,
              message = "Can't add base type '" & baseTypeIndex &
              "', recipe with index '" & recipeIndex & "' already added.")
        if subAction == DataAction.remove:
          for index, recipe in baseType.recipes.pairs:
            if recipe == recipeIndex:
              baseType.recipes.delete(i = index)
              break
        else:
          baseType.recipes.add(y = recipeIndex)
      of "flag":
        let
          flagName = childNode.attr(name = "name")
          subAction = try:
              parseEnum[DataAction](childNode.attr(
                  name = "action").toLowerAscii)
            except ValueError:
              DataAction.add
        if subAction == DataAction.add and flagName in baseType.flags:
          raise newException(exceptn = DataLoadingError,
              message = "Can't add base type '" & baseTypeIndex &
              "', flag '" & flagName & "' already added.")
        if subAction == DataAction.remove:
          for index, flag in baseType.flags.pairs:
            if flag == flagName:
              baseType.flags.delete(i = index)
              break
        else:
          baseType.flags.add(y = flagName)
    basesTypesList[baseTypeIndex] = baseType


type
  AdaBaseTypeData* = object
    name: cstring
    color: cstring
    description: cstring

  AdaPricesArray* = array[1..2, cint]

proc loadAdaBasesTypes(fileName: cstring) {.exportc.} =
  loadBasesTypes(fileName = $fileName)

proc getAdaBaseType(index: cstring; adaBaseType: var AdaBaseTypeData) {.sideEffect,
    raises: [], tags: [], exportc.} =
  adaBaseType = AdaBaseTypeData(name: "".cstring, color: "".cstring,
      description: "".cstring)
  let baseTypeKey = strip(s = $index)
  if not basesTypesList.hasKey(key = baseTypeKey):
    return
  let baseType = try:
      basesTypesList[baseTypeKey]
    except KeyError:
      return
  adaBaseType.name = baseType.name.cstring
  adaBaseType.color = baseType.color.cstring
  adaBaseType.description = baseType.description.cstring

proc getAdaBaseData(baseIndex: cstring; index: cint;
    adaDataType: cstring): cstring {.exportc.} =
  let baseTypeKey = strip(s = $baseIndex)
  if not basesTypesList.hasKey(key = baseTypeKey):
    return ""
  let dataList = if adaDataType == "recipe":
        basesTypesList[baseTypeKey].recipes
      else:
        basesTypesList[baseTypeKey].flags
  if index >= dataList.len():
    return ""
  return dataList[index].cstring

proc getAdaBaseTrade(baseIndex: cstring; index: cint;
    adaBaseTrade: var AdaPricesArray): cstring {.exportc.} =
  adaBaseTrade = [1: 0.cint, 2: 0.cint]
  let baseTypeKey = strip(s = $baseIndex)
  if not basesTypesList.hasKey(key = baseTypeKey):
    return ""
  if index > basesTypesList[baseTypeKey].trades.len():
    return ""
  var currIndex = 1
  for tradeIndex, trade in basesTypesList[baseTypeKey].trades.pairs:
    currIndex.inc()
    if currIndex < index:
      continue
    adaBaseTrade = [1: trade[1].cint, 2: trade[2].cint]
    let newIndex = $tradeIndex
    return newIndex.cstring
