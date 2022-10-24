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
  PricesArray* = array[1..2, Natural]

  BaseTypeData* = object
    name: string
    color: string
    trades: Table[string, PricesArray]
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

proc loadAdaBasesTypes(fileName: cstring) {.exportc.} =
  loadBasesTypes(fileName = $fileName)
