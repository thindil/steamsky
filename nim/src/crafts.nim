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
          CraftData(time: 1, difficulty: 1, toolQuality: 1)
      else:
        CraftData(time: 1, difficulty: 1, toolQuality: 1)
    for material in recipeNode.findAll(tag = "material"):
      let
        amount = try:
            material.attr(name = "amount").parseInt()
          except ValueError:
            0
        materialType = material.attr(name = "type")
      if amount > 0:
        if materialType notin recipe.materialTypes:
          recipe.materialTypes.add(y = materialType)
          recipe.materialAmounts.add(y = amount)
      else:
        var deleteIndex: Natural = 0
        for index, mType in recipe.materialTypes.pairs:
          if mType == materialType:
            deleteIndex = index
            break
        recipe.materialTypes.delete(i = deleteIndex)
        {.warning[UnsafeSetLen]: off.}
        recipe.materialAmounts.delete(i = deleteIndex)
        {.warning[UnsafeSetLen]: on.}
    var attribute = recipeNode.attr(name = "result")
    if attribute.len() > 0:
      recipe.resultIndex = try:
        attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError, message = "Can't " &
            $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe result index.")
    attribute = recipeNode.attr(name = "crafted")
    if attribute.len() > 0:
      recipe.resultAmount = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe result amount.")
    attribute = recipeNode.attr(name = "workplace")
    if attribute.len() > 0:
      recipe.workplace = try:
          parseEnum[ModuleType](attribute.toLowerAscii)
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe workplace.")
    attribute = recipeNode.attr(name = "skill")
    if attribute.len() > 0:
      let skillIndex = findSkillIndex(skillName = attribute)
      if skillIndex == 0:
        raise newException(exceptn = DataLoadingError, message = "Can't " &
            $recipeAction & " recipe '" & $recipeIndex & "', no skill named '" &
            attribute & "'.")
      recipe.skill = skillIndex
    attribute = recipeNode.attr(name = "time")
    if attribute.len() > 0:
      recipe.time = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe time.")
    attribute = recipeNode.attr(name = "difficulty")
    if attribute.len() > 0:
      recipe.difficulty = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe difficulty.")
    attribute = recipeNode.attr(name = "tool")
    if attribute.len() > 0:
      recipe.tool = attribute
    attribute = recipeNode.attr(name = "reputation")
    if attribute.len() > 0:
      recipe.reputation = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe required reputation.")
    attribute = recipeNode.attr(name = "Tool_Quality")
    if attribute.len() > 0:
      recipe.toolQuality = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe tool quality.")
    if recipeAction == DataAction.add:
      logMessage(message = "Recipe added: '" & $recipeIndex & "'",
          debugType = everything)
    else:
      logMessage(message = "Recipe updated: '" & $recipeIndex & "'",
          debugType = everything)
    recipesList[recipeIndex] = recipe

proc loadAdaRecipes(fileName: cstring) {.exportc.} =
  loadRecipes(fileName = $fileName)
