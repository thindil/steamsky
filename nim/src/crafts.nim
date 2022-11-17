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
    ## FUNCTION
    ##
    ## Used to store information about crafting recipes
    materialTypes: seq[string] ## The list of materials types used in crafting
    materialAmounts: seq[Positive] ## The list of materials amount used in crafting
    resultIndex: Natural ## The index of proto item which is the result of the recipe
    resultAmount: Natural ## The amount of items produced by one recipe
    workplace: ModuleType ## The type of ship's module used as a workshop for the recipe
    skill: Natural ## The index of the skill used in crafting
    time: Positive ## The amount of minutes needed to finish the recipe
    difficulty: Positive ## The difficulty level of the recipe
    tool: string ## The type of item used as a tool in crafting
    reputation: ReputationRange ## The minimal amount of reputation needed to buy the recipe in bases
    toolQuality: Positive ## The minimal quality of tool used in crafting

var recipesList* = initTable[string, CraftData]()
  ## FUNCTION
  ##
  ## The list of all available crafting recipes in the game

proc loadRecipes*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## FUNCTION
  ##
  ## Load the crafting recipes data from the file
  ##
  ## PARAMETERS
  ##
  ## * fileName - the name of the file to load
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
    else:
      recipe.time = 15
    attribute = recipeNode.attr(name = "difficulty")
    if attribute.len() > 0:
      recipe.difficulty = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe difficulty.")
    else:
      recipe.difficulty = 1
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
    else:
      recipe.reputation = -100
    attribute = recipeNode.attr(name = "toolquality")
    if attribute.len() > 0:
      recipe.toolQuality = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $recipeAction & " recipe '" & $recipeIndex & "', invalid value for recipe tool quality.")
    else:
      recipe.toolQuality = 100
    if recipeAction == DataAction.add:
      logMessage(message = "Recipe added: '" & $recipeIndex & "'",
          debugType = everything)
    else:
      logMessage(message = "Recipe updated: '" & $recipeIndex & "'",
          debugType = everything)
    recipesList[recipeIndex] = recipe

# Temporary code for interfacing with Ada

type
  AdaCraftData = object
    resultIndex: cint
    resultAmount: cint
    workplace: cint
    skill: cint
    time: cint
    difficulty: cint
    tool: cstring
    reputation: cint
    toolQuality: cint

proc loadAdaRecipes(fileName: cstring) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  loadRecipes(fileName = $fileName)

proc getAdaCraftData(index: cstring; adaRecipe: var AdaCraftData) {.sideEffect,
    raises: [], tags: [], exportc.} =
  adaRecipe = AdaCraftData(resultIndex: 0, resultAmount: 0, workplace: 0,
      skill: 0, time: 1, difficulty: 1, tool: "".cstring, reputation: -100,
      toolQuality: 1)
  let recipeKey = strip(s = $index)
  if not recipesList.hasKey(key = recipeKey):
    return
  let recipe = try:
      recipesList[recipeKey]
    except KeyError:
      return
  adaRecipe.resultIndex = recipe.resultIndex.cint
  adaRecipe.resultAmount = recipe.resultAmount.cint
  adaRecipe.workplace = recipe.workplace.ord().cint
  adaRecipe.skill = recipe.skill.cint
  adaRecipe.time = recipe.time.cint
  adaRecipe.difficulty = recipe.difficulty.cint
  adaRecipe.tool = recipe.tool.cstring
  adaRecipe.toolQuality = recipe.toolQuality.cint

proc getAdaRecipeMaterialType(recipeIndex: cstring;
    index: cint): cstring {.sideEffect, raises: [], tags: [], exportc.} =
  try:
    if index >= recipesList[$recipeIndex].materialTypes.len():
      return ""
    return recipesList[$recipeIndex].materialTypes[index].cstring
  except KeyError:
    return ""

proc getAdaRecipeMaterialAmount(recipeIndex: cstring;
    index: cint): cint {.sideEffect, raises: [], tags: [], exportc.} =
  try:
    if index >= recipesList[$recipeIndex].materialAmounts.len():
      return 0
    return recipesList[$recipeIndex].materialAmounts[index].cint
  except KeyError:
    return 0
