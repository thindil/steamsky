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
import game, log, shipmodules

type
  CraftData = object
    materialTypes: seq[string]
    materialAmounts: seq[Positive]
    resultIndex: Natural
    resultAmount: Natural
    workplace: ModuleType
    skill: Natural
    time: Positive
    difficulty: Positive
    tool: string
    reputation: ReputationRange
    toolQuality: Positive

var recipesList* = initTable[string, CraftData]()

proc loadRecipes*(fileName: string) =
  let recipesXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load crafting recipes data file. Reason: " &
          getCurrentExceptionMsg())
  for recipeNode in recipesXml:
    if recipeNode.kind != xnElement:
      continue
    let
      recipeIndex: string = recipeNode.attr(name = "index")
      recipeAction: DataAction = try:
          parseEnum[DataAction](recipeNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if recipeAction in [update, remove]:
      if not recipesList.hasKey(key = recipeIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $recipeAction & " recipe '" & $recipeIndex & "', there is no recipe with that index.")
    elif recipesList.hasKey(key = recipeIndex):
      raise newException(exceptn = DataLoadingError,
          message = "Can't add recipe '" & $recipeIndex & "', there is a recipe with that index.")
    if recipeAction == DataAction.remove:
      {.warning[ProveInit]: off.}
      {.warning[UnsafeDefault]: off.}
      recipesList.del(key = recipeIndex)
      {.warning[ProveInit]: on.}
      {.warning[UnsafeDefault]: on.}
      logMessage(message = "Recipe removed: '" & $recipeIndex & "'",
          debugType = everything)
      continue
    var recipe: CraftData = if recipeAction == DataAction.update:
        try:
          recipesList[recipeIndex]
        except ValueError:
          CraftData(time: 1, difficulty:1, toolQuality: 1)
      else:
        CraftData(time: 1, difficulty:1, toolQuality: 1)
