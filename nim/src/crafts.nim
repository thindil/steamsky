# Copyright 2022-2023 Bartek thindil Jasicki
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

import std/[strutils, tables, xmlparser, xmltree]
import crewinventory, game, items, log, messages, ships, shipscargo, shipscrew,
    statistics, trades, types

type
  CraftingNoWorkshopError* = object of CatchableError
    ## Used to mark problems during crafting with lack of proper workshop

  CraftingNoMaterialsError* = object of CatchableError
    ## Used to mark problems during crafting with lack of proper crafting materials

  CraftingNoToolsError* = object of CatchableError
    ## Used to mark problems during crafting with lack of proper crafting tools

var knownRecipes: seq[string] ## The list of known recipes by the player

proc loadRecipes*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## Load the crafting recipes data from the file
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

proc setRecipeData*(recipeIndex: string): CraftData {.sideEffect, raises: [
    KeyError, ValueError], tags: [].} =
  ## Set the crafting data for the selected recipe
  ##
  ## * recipeIndex - index of the recipe which data will be set or full action
  ##                 name related to the recipe, like "Study 12"
  ##
  ## Returns CraftData object with information about the crafting recipe
  result = CraftData(time: 15, difficulty: 1, toolQuality: 100)
  var itemIndex = 0
  if recipeIndex.len > 6 and recipeIndex[0..4] == "Study":
    itemIndex = recipeIndex[6..^1].strip.parseInt
    result.materialTypes.add(y = itemsList[itemIndex].itemType)
    result.materialAmounts.add(y = 1)
    result.resultIndex = itemIndex
    result.resultAmount = 0
    result.workplace = alchemyLab
    for recipe in recipesList.values:
      if recipe.resultIndex == result.resultIndex:
        result.skill = recipe.skill
        result.time = recipe.difficulty * 15
        break
    result.tool = alchemyTools
    return
  elif recipeIndex.len > 12 and recipeIndex[0..10] == "Deconstruct":
    itemIndex = recipeIndex[12..^1].strip.parseInt
    result.materialTypes.add(y = itemsList[itemIndex].itemType)
    result.materialAmounts.add(y = 1)
    result.workplace = alchemyLab
    for recipe in recipesList.values:
      if recipe.resultIndex == itemIndex:
        result.skill = recipe.skill
        result.time = recipe.difficulty * 15
        result.resultIndex = findProtoItem(itemType = recipe.materialTypes[0])
        result.resultAmount = (recipe.materialAmounts[0].float * 0.8).int
        if result.resultAmount == recipe.resultAmount:
          result.resultAmount.dec
        if result.resultAmount == 0:
          result.resultAmount = 1
        break
    result.tool = alchemyTools
    return
  return recipesList[recipeIndex]

proc checkRecipe*(recipeIndex: string): Positive {.sideEffect, raises: [
    ValueError, CraftingNoWorkshopError, CraftingNoMaterialsError,
    CraftingNoToolsError, TradeNoFreeCargoError], tags: [].} =
  ## Check if player have all requirements for the selected recipe
  ##
  ## * recipeIndex - index of the recipe which data will be set or full action
  ##                 name related to the recipe, like "Study 12"
  ##
  ## Returns the maximum amount of items which can be crafted with the selected
  ## recipe
  let recipe = setRecipeData(recipeIndex = recipeIndex)
  var
    recipeName = ""
    itemIndex = 0
    mType: ModuleType
  if recipeIndex.len > 6 and recipeIndex[0..4] == "Study":
    itemIndex = recipeIndex[6..^1].strip.parseInt
    recipeName = "studying " & itemsList[itemIndex].name
    mType = alchemyLab
  elif recipeIndex.len > 12 and recipeIndex[0..10] == "Deconstruct":
    itemIndex = recipeIndex[12..^1].strip.parseInt
    recipeName = "deconstructing " & itemsList[itemIndex].name
    mType = alchemyLab
  else:
    recipeName = "manufacturing " & itemsList[recipe.resultIndex].name
    mType = recipesList[recipeIndex].workplace
  var haveWorkshop = false
  for module in playerShip.modules:
    if modulesList[module.protoIndex].mType == mType and module.durability > 0:
      haveWorkshop = true
      break
  if not haveWorkshop:
    raise newException(exceptn = CraftingNoWorkshopError, message = recipeName)
  result = Positive.high
  var materialIndexes: seq[Natural]
  if recipeIndex.len > 6 and recipeIndex[0..4] == "Study":
    for i in playerShip.cargo.low..playerShip.cargo.high:
      if itemsList[playerShip.cargo[i].protoIndex].name == itemsList[
          recipe.resultIndex].name:
        materialIndexes.add(y = i)
        break
    result = 1
  elif recipeIndex.len > 12 and recipeIndex[0..10] == "Deconstruct":
    for i in playerShip.cargo.low..playerShip.cargo.high:
      if playerShip.cargo[i].protoIndex == itemIndex:
        materialIndexes.add(y = i)
        result = playerShip.cargo[i].amount
        break
  else:
    for j in recipe.materialTypes.low..recipe.materialTypes.high:
      for i in playerShip.cargo.low..playerShip.cargo.high:
        if itemsList[playerShip.cargo[i].protoIndex].itemType ==
            recipe.materialTypes[j] and playerShip.cargo[i].amount >=
            recipe.materialAmounts[j]:
          materialIndexes.add(y = i)
          if result > (playerShip.cargo[i].amount / recipe.materialAmounts[j]).Positive:
            result = (playerShip.cargo[i].amount / recipe.materialAmounts[j]).Positive
            break
  if materialIndexes.len < recipe.materialTypes.len:
    raise newException(exceptn = CraftingNoMaterialsError, message = recipeName)
  var haveTool = false
  if recipe.tool != "None" and findItem(inventory = playerShip.cargo,
      itemType = recipe.tool, quality = recipe.toolQuality) > 0:
    haveTool = true
    if not haveTool:
      raise newException(exceptn = CraftingNoToolsError, message = recipeName)
  var spaceNeeded = 0
  for i in materialIndexes.low..materialIndexes.high:
    spaceNeeded = spaceNeeded + (itemsList[playerShip.cargo[materialIndexes[
        i]].protoIndex].weight * recipe.materialAmounts[i])
    if freeCargo(amount = spaceNeeded - (itemsList[recipe.resultIndex].weight *
        recipe.resultAmount)) < 0:
      raise newException(exceptn = TradeNoFreeCargoError, message = "")

proc manufacturing*(minutes: Positive) =
  var toolIndex, crafterIndex: int

  proc resetOrder(module: var ModuleData; moduleOwner: int) =
    if toolIndex in 0..playerShip.crew[crafterIndex].inventory.high:
      updateCargo(ship = playerShip, protoIndex = playerShip.crew[
          crafterIndex].inventory[toolIndex].protoIndex, amount = 1,
          durability = playerShip.crew[crafterIndex].inventory[
          toolIndex].durability)
      updateInventory(memberIndex = crafterIndex, amount = -1,
          inventoryIndex = toolIndex, ship = playerShip)
    var haveWorker = false
    for i in module.owner.low..module.owner.high:
      if module.owner[i] == moduleOwner or moduleOwner == -1:
        if module.owner[i] in 0..playerShip.crew.high:
          giveOrders(ship = playerShip, memberIndex = module.owner[i],
              givenOrder = rest)
        module.owner[i] = -1
      if module.owner[i] > -1:
        haveWorker = true
    if not haveWorker:
      module.craftingIndex = ""
      module.craftingTime = 0
      module.craftingAmount = 0

  for module in playerShip.modules.mitems:
    if module.mType != ModuleType2.workshop:
      continue
    if module.craftingIndex.len == 0:
      continue
    for owner in module.owner:
      if owner == -1:
        continue
      crafterIndex = owner
      if playerShip.crew[crafterIndex].order == craft:
        var
          currentMinutes = minutes
          recipeTime = module.craftingTime
          recipeName = ""
        let recipe = setRecipeData(recipeIndex = module.craftingIndex)
        if module.craftingIndex.len > 6 and module.craftingIndex[0..4] == "Study":
          recipeName = "studying " & itemsList[recipe.resultIndex].name
        elif module.craftingIndex.len > 12 and module.craftingIndex[0..10] == "Deconstruct":
          recipeName = "deconstructing " & itemsList[module.craftingIndex[
              12..^1].strip.parseInt].name
        else:
          recipeName = "manufacturing " & itemsList[recipe.resultIndex].name
        if module.durability == 0:
          addMessage(message = module.name & " is destroyed, so " &
              playerShip.crew[crafterIndex].name & " can't work on " &
              recipeName & ".", mType = craftMessage, color = red)
          resetOrder(module = module, moduleOwner = owner)
          currentMinutes = 0
        var
          workTime = playerShip.crew[crafterIndex].orderTime
          craftedAmount = 0
        while currentMinutes > 0:
          if currentMinutes < recipeTime:
            recipeTime.dec(y = currentMinutes)
            workTime.dec(y = currentMinutes)
            currentMinutes = 0
            break
          recipeTime = recipeTime - currentMinutes
          workTime = workTime - currentMinutes - recipeTime
          currentMinutes = 0 - recipeTime
          recipeTime = recipe.time
          var materialIndexes: seq[Positive]
          if module.craftingIndex.len > 6 and module.craftingIndex[0..4] == "Study":
            for j in 1..itemsList.len:
              if itemsList[j].name == itemsList[recipe.resultIndex].name:
                materialIndexes.add(y = j)
                break
          elif module.craftingIndex.len > 12 and module.craftingIndex[1..10] == "Deconstruct":
            materialIndexes.add(y = module.craftingIndex[12..^1].strip.parseInt)
          else:
            for materialType in recipe.materialTypes:
              for j in 1..itemsList.len:
                if itemsList[j].itemType == materialType:
                  materialIndexes.add(y = j)
                  break
          var craftingMaterial = -1
          for materialIndex in materialIndexes.mitems:
            craftingMaterial = findItem(inventory = playerShip.cargo,
                itemType = itemsList[materialIndex].itemType)
            if craftingMaterial == -1:
              addMessage(message = "You don't have the crafting materials for " &
                  recipeName & ".", mType = craftMessage, color = red)
              resetOrder(module = module, moduleOwner = owner)
              break
            elif playerShip.cargo[craftingMaterial].protoIndex != materialIndex:
              materialIndex = playerShip.cargo[craftingMaterial].protoIndex
          if craftingMaterial == -1:
            break
          var toolIndex = -1
          if recipe.tool == "None":
            toolIndex = -1
          else:
            toolIndex = findTools(memberIndex = crafterIndex,
                itemType = recipe.tool, order = craft,
                toolQuality = recipe.toolQuality)
            if toolIndex == -1:
              addMessage(message = "You don't have the tool for " & recipeName &
                  ".", mType = craftMessage, color = red)
              break
          var amount = 0
          for j in 0..materialIndexes.high:
            amount = amount + (itemsList[materialIndexes[j]].weight *
                recipe.materialAmounts[j])
          var resultAmount = recipe.resultAmount + (recipe.resultAmount.float *
              (getSkillLevel(member = playerShip.crew[crafterIndex],
              skillIndex = recipe.skill).float / 100.0)).int
          let damage = 1.0 - (module.durability.float /
              module.maxDurability.float)
          resultAmount = resultAmount - (resultAmount.float * damage).int
          if resultAmount == 0:
            resultAmount = 1
          var haveMaterial = false
          for j in 0..materialIndexes.high:
            haveMaterial = false
            for item in playerShip.cargo:
              if itemsList[item.protoIndex].itemType == itemsList[
                  materialIndexes[j]].itemType and item.amount >=
                      recipe.materialAmounts[j]:
                haveMaterial = true
                break
            if not haveMaterial:
              break
          if not haveMaterial:
            addMessage(message = "You don't have enough crafting materials for " &
                recipeName & ".", mType = craftMessage, color = red)
            resetOrder(module = module, moduleOwner = owner)
            break
          craftedAmount = craftedAmount + resultAmount
          module.craftingAmount.dec
          for j in 0..materialIndexes.high:
            var cargoIndex = 0
            while cargoIndex <= playerShip.cargo.high:
              var material = playerShip.cargo[cargoIndex]
              if itemsList[material.protoIndex].itemType == itemsList[
                  materialIndexes[j]].itemType:
                if material.amount > recipe.materialAmounts[j]:
                  let newAmount = material.amount - recipe.materialAmounts[j]
                  material.amount = newAmount
                  playerShip.cargo[cargoIndex] = material
                  break
                elif material.amount == recipe.materialAmounts[j]:
                  playerShip.cargo.delete(i = cargoIndex)
                  if toolIndex > cargoIndex:
                    toolIndex.dec
                  break
            cargoIndex.inc
          if toolIndex > -1:
            damageItem(inventory = playerShip.crew[crafterIndex].inventory,
                itemIndex = toolIndex, skillLevel = getSkillLevel(
                member = playerShip.crew[crafterIndex],
                skillIndex = recipe.skill), memberIndex = crafterIndex,
                ship = playerShip)
          if module.craftingIndex.len < 6 or (module.craftingIndex.len > 6 and
              module.craftingIndex[0..4] != "Study"):
            amount = amount - (itemsList[recipe.resultIndex].weight * resultAmount)
            if freeCargo(amount = amount) < 0:
              addMessage(message = "You don't have the free cargo space for " &
                  recipeName, mType = craftMessage, color = red)
              resetOrder(module = module, moduleOwner = owner)
              break
            if module.craftingIndex.len > 11 and module.craftingIndex[0..10] == "Deconstruct":
              updateCargo(ship = playerShip, protoIndex = recipe.resultIndex,
                  amount = resultAmount)
            else:
              updateCargo(ship = playerShip, protoIndex = recipesList[
                  module.craftingIndex].resultIndex, amount = resultAmount)
            for key, recipe in recipesList.pairs:
              if recipe.resultIndex == recipe.resultIndex:
                updateCraftingOrders(index = key)
                break
          else:
            for key, recipe in recipesList.pairs:
              if recipe.resultIndex == recipe.resultIndex:
                knownRecipes.add(y = key)
        module.craftingTime = recipeTime
        if craftedAmount > 0:
          if recipe.resultAmount > 0:
            if module.craftingIndex.len > 12 and module.craftingIndex[0..10] == "Deconstruct":
              addMessage(message = playerShip.crew[crafterIndex].name &
                  " has recovered " & $craftedAmount & " " & itemsList[
                  recipe.resultIndex].name & ".", mType = craftMessage, color = green)
            else:
              addMessage(message = playerShip.crew[crafterIndex].name &
                  " has manufactured " & $craftedAmount & " " & itemsList[
                  recipe.resultIndex].name & ".", mType = craftMessage, color = green)

proc setRecipe*(workshop: Natural; amount: Positive;
    recipeIndex: string) {.sideEffect, raises: [ValueError, CrewOrderError,
    CrewNoSpaceError, Exception], tags: [RootEffect].} =
  ## Set the selected crafting recipe for the selected workshop in the player's
  ## ship
  ##
  ## * workshop    - the index of the workshop in which the recipe will be set
  ## * amount      - how many times the recipe will be made
  ## * recipeIndex - the index of the crafting recipe to set
  playerShip.modules[workshop].craftingAmount = amount
  var
    itemIndex = 0
    recipeName = ""
  if recipeIndex.len > 6 and recipeIndex[0..4] == "Study":
    itemIndex = recipeIndex[6..^1].strip.parseInt
    for recipe in recipesList.values:
      if recipe.resultIndex == itemIndex:
        playerShip.modules[workshop].craftingTime = recipe.difficulty * 15
        break
    recipeName = "Studying " & itemsList[itemIndex].name
    playerShip.modules[workshop].craftingIndex = recipeIndex
  elif recipeIndex.len > 12 and recipeIndex[0..10] == "Deconstruct":
    itemIndex = recipeIndex[12..^1].strip.parseInt
    for recipe in recipesList.values:
      if recipe.resultIndex == itemIndex:
        playerShip.modules[workshop].craftingTime = recipe.difficulty * 15
        break
    recipeName = "Deconstructing " & itemsList[itemIndex].name
    playerShip.modules[workshop].craftingIndex = recipeIndex
  else:
    playerShip.modules[workshop].craftingIndex = recipeIndex.strip
    playerShip.modules[workshop].craftingTime = recipesList[recipeIndex].time
    recipeName = itemsList[recipesList[recipeIndex].resultIndex].name
  addMessage(message = (recipeName & " was set as manufacturing order in " &
      playerShip.modules[workshop].name & ".").cstring,
      kind = orderMessage.ord.cint)
  updateOrders(ship = playerShip)

proc getWorkshopRecipeName*(workshop: Natural): string {.sideEffect, raises: [
    KeyError, ValueError], tags: [].} =
  ## Get the name of the recipe set as working order for the selected workshop
  ##
  ## * workshop - the index of the workshop which crafting order recipe name
  ##              will be get
  ##
  ## Returns the name of the recipe set as the crafting order or empty string if nothing
  ## is set
  let module = playerShip.modules[workshop]
  if module.craftingIndex.len > 0:
    if module.craftingIndex.len > 6 and module.craftingIndex[0..4] == "Study":
      return "Studying " & itemsList[module.craftingIndex[
          6..^1].strip.parseInt].name
    elif module.craftingIndex.len > 12 and module.craftingIndex[0..10] == "Deconstruct":
      return "Deconstructing " & itemsList[module.craftingIndex[
          12..^1].strip.parseInt].name
    else:
      return "Manufacturing " & $module.craftingAmount & "x " & itemsList[
          recipesList[module.craftingIndex].resultIndex].name
  return ""

# Temporary code for interfacing with Ada

type
  AdaCraftData = object
    materialTypes: array[0..4, cstring]
    materialAmounts: array[0..4, cint]
    resultIndex: cint
    resultAmount: cint
    workplace: cint
    skill: cint
    time: cint
    difficulty: cint
    tool: cstring
    reputation: cint
    toolQuality: cint

proc loadAdaRecipes(fileName: cstring): cstring {.sideEffect, raises: [],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  try:
    loadRecipes(fileName = $fileName)
    return "".cstring
  except DataLoadingError:
    return getCurrentExceptionMsg().cstring

proc getAdaCraftData(index: cstring; adaRecipe: var AdaCraftData) {.sideEffect,
    raises: [], tags: [], exportc.} =
  adaRecipe = AdaCraftData(resultIndex: 0, resultAmount: 0, workplace: 0,
      skill: 0, time: 1, difficulty: 1, tool: "".cstring, reputation: -100,
      toolQuality: 1)
  for i in 0..4:
    adaRecipe.materialTypes[i] = "".cstring
    adaRecipe.materialAmounts[i] = 0
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
  adaRecipe.reputation = recipe.reputation.cint
  for i in 0..recipe.materialTypes.high:
    adaRecipe.materialTypes[i] = recipe.materialTypes[i].cstring
    adaRecipe.materialAmounts[i] = recipe.materialAmounts[i].cint

proc getAdaWorkshopRecipeName(workshop: cint): cstring {.raises: [], tags: [], exportc.} =
  try:
    return getWorkshopRecipeName(workshop).cstring
  except ValueError:
    return "".cstring

proc setAdaRecipe(workshop, amount: cint; recipeIndex: cstring) {.raises: [],
    tags: [RootEffect], exportc.} =
  try:
    setRecipe(workshop = workshop.Natural - 1, amount = amount.Positive,
        recipeIndex = $recipeIndex)
  except ValueError, Exception:
    discard

proc setAdaRecipeData(recipeIndex: cstring;
    adaRecipe: var AdaCraftData) {.raises: [], tags: [], exportc.} =
  adaRecipe = AdaCraftData(resultIndex: 0, resultAmount: 0, workplace: 0,
      skill: 0, time: 1, difficulty: 1, tool: "".cstring, reputation: -100,
      toolQuality: 1)
  for i in 0..4:
    adaRecipe.materialTypes[i] = "".cstring
    adaRecipe.materialAmounts[i] = 0
  try:
    let recipe = setRecipeData(recipeIndex = $recipeIndex)
    adaRecipe.resultIndex = recipe.resultIndex.cint
    adaRecipe.resultAmount = recipe.resultAmount.cint
    adaRecipe.workplace = recipe.workplace.ord().cint
    adaRecipe.skill = recipe.skill.cint
    adaRecipe.time = recipe.time.cint
    adaRecipe.difficulty = recipe.difficulty.cint
    adaRecipe.tool = recipe.tool.cstring
    adaRecipe.toolQuality = recipe.toolQuality.cint
    adaRecipe.reputation = recipe.reputation.cint
    for i in 0..recipe.materialTypes.high:
      adaRecipe.materialTypes[i] = recipe.materialTypes[i].cstring
      adaRecipe.materialAmounts[i] = recipe.materialAmounts[i].cint
  except ValueError:
    discard

proc checkAdaRecipe(recipeIndex: cstring): cint {.raises: [], tags: [], exportc.} =
  try:
    return checkRecipe(recipeIndex = $recipeIndex).cint
  except ValueError:
    return 0
  except TradeNoFreeCargoError:
    return -1
  except CraftingNoWorkshopError:
    return -2
  except CraftingNoMaterialsError:
    return -3
  except CraftingNoToolsError:
    return -4
