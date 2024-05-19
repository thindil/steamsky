# Copyright 2024 Bartek thindil Jasicki
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

import std/[os, strutils, tables]
import ../[config, crafts, crewinventory, game, tk, types]
import coreui, table, utilsui2

proc checkTool(toolNeeded: string): bool {.sideEffect, raises: [], tags: [].} =
  ##  Check if the player has needed tool for the crafting recipe
  ##
  ##  * toolNeeded - The type of tool needed for the recipe
  ##
  ## Returns true if the tool is in the player ship cargo, otherwise false
  result = true
  if toolNeeded != "None":
    result = false
    for index, item in itemsList:
      if item.itemType == toolNeeded:
        let cargoIndex = findItem(inventory = playerShip.cargo,
            protoIndex = index)
        if cargoIndex > -1:
          result = true
          break

proc isCraftable(recipe: CraftData; canCraft, hasWorkplace, hasTool,
    hasMaterials: var bool) {.sideEffect, raises: [], tags: [].} =
  ## Check if the selected recipe can be crafted (has all requirements meet)
  ##
  ## * recipe       - The crafting recipe to check
  ## * canCraft     - If recipe can be crafted, then it will be true, otherwise
  ##                  false
  ## * hasWorkplace - If there is workplace for the recipe, will be true,
  ##                  otherwise false
  ## * hasTool      - If there is available tool for the recipe, will be true,
  ##                  otherwise false
  ## * hasMaterials - If there are available materials for the recipe, will be
  ##                  true, otherwise false
  ##
  ## Returns parameters canCraft, hasWorkplace, hasTool, hasMaterials
  canCraft = false
  hasWorkplace = false
  hasMaterials = false
  hasTool = false
  for module in playerShip.modules:
    try:
      if modulesList[module.protoIndex].mType == recipe.workplace and
          module.durability > 0:
        hasWorkplace = true
        break
    except:
      showError(message = "Can't check workshop.")
      return
  hasTool = checkTool(toolNeeded = recipe.tool)
  for materialIndex, material in recipe.materialTypes:
    hasMaterials = false
    for itemIndex, item in itemsList:
      if item.itemType == material:
        var cargoIndex = findItem(inventory = playerShip.cargo,
            protoIndex = itemIndex)
        if cargoIndex > -1 and playerShip.cargo[cargoIndex].amount >=
            recipe.materialAmounts[materialIndex]:
          hasMaterials = true
  if hasTool and hasMaterials and hasWorkplace:
    canCraft = true

proc checkStudyPrerequisities(canCraft, hasTool,
    hasWorkplace: var bool) {.sideEffect, raises: [], tags: [].} =
  ## Check if the study and decontruct recipes can be crafted
  ##
  ## * canCraft      - If recipe can be crafter then it will be True, otherwise
  ##                   False
  ## * hasTool       - If there is tool for the study and deconstruct recipes
  ##                   then True, otherwise False
  ## * hasWorkplace  - If there is workplace for study and deconstruct recipes
  ##                   then True, otherwise False
  ##
  ## Returns parameters canCraft, hasTool and hasWorkplace
  hasTool = checkTool(toolNeeded = alchemyTools)
  canCraft = false
  hasWorkplace = false
  for module in playerShip.modules:
    try:
      if modulesList[module.protoIndex].mType == alchemyLab and
          module.durability > 0:
        hasWorkplace = true
        break
    except:
      showError(message = "Can't check workshop.")
      return
  if hasWorkplace:
    canCraft = true

var
  studies: seq[Positive]
  deconstructs: seq[Positive]
  recipesIndexes: seq[string]
  recipesTable: TableWidget

proc showCraftingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  var craftsFrame = mainPaned & ".craftframe"
  let craftsCanvas = craftsFrame & ".canvas"
  if tclEval2(script = "winfo exists " & craftsCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "crafts.tcl")
    tclEval(script = "bind " & craftsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & craftsCanvas) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "crafts")
  let
    recipeName = (if argc == 3: $argv[2] else: "")
    searchEntry = craftsCanvas & ".crafts.sframe.search"
  if recipeName.len == 0:
    tclEval(script = searchEntry & " configure -validatecommand {}")
    tclEval(script = searchEntry & " delete 0 end")
    tclEval(script = searchEntry & " configure -validatecommand {ShowCrafting 1 %P}")
  studies = @[]
  deconstructs = @[]
  for item in playerShip.cargo:
    for recipeIndex, recipe in recipesList:
      if recipe.resultIndex == item.protoIndex:
        if recipeIndex notin knownRecipes and item.protoIndex notin studies:
          studies.add(y = item.protoIndex)
        if recipe.materialAmounts[0] > 1 and recipe.resultAmount == 1:
          deconstructs.add(y = item.protoIndex)
  if recipesIndexes.len != knownRecipes.len + studies.len + deconstructs.len:
    recipesIndexes = @[]
    for recipe in knownRecipes:
      recipesIndexes.add(y = recipe)
    for recipe in studies:
      recipesIndexes.add(y = $recipe)
    for recipe in deconstructs:
      recipesIndexes.add(y = $recipe)
  if recipesTable.rowHeight == 0:
    recipesTable = createTable(parent = craftsCanvas & ".craft", headers = @[
        "Name", "Workshop", "Tools", "Materials"], scrollbar = craftsFrame &
        ".scrolly", command = "SortCrafting",
        tooltipText = "Press mouse button to sort the crafting recipes.")
  else:
    recipesTable.clearTable
  let
    typeBox = craftsCanvas & ".craft.sframe.show"
    showType = tclEval2(script = typeBox & " current").parseInt + 1
    page = (if argc == 2: ($argv[1]).parseInt else: 1)
    startRow = (page - 1) * gameSettings.listsLimit + 1
  var
    currentRow = 1
    canCraft, hasWorkplace, hasTool, hasMaterials = false
  for index, rec in recipesIndexes:
    if index > knownRecipes.high:
      break
    if recipeName.len > 0 and itemsList[recipesList[
        rec].resultIndex].name.toLowerAscii.find(sub = recipeName.toLowerAscii,
        start = 1) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let recipe = recipesList[rec]
    isCraftable(recipe = recipe, canCraft = canCraft,
        hasWorkplace = hasWorkplace, hasTool = hasTool,
        hasMaterials = hasMaterials)
    if (showType == 2 and not canCraft) or (showType == 3 and canCraft):
      continue
    addButton(table = recipesTable, text = itemsList[recipe.resultIndex].name,
        tooltip = "Show recipe's details", command = "ShowRecipeInfo {" & rec &
        "} " & $canCraft, column = 1)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {" & rec & "} " & $canCraft,
        checked = hasWorkplace, column = 2)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {" & rec & "} " & $canCraft,
        checked = hasTool, column = 3)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {" & rec & "} " & $canCraft,
        checked = hasMaterials, column = 4, newRow = true)
    if recipesTable.row == gameSettings.listsLimit + 1:
      break
  checkStudyPrerequisities(canCraft = canCraft, hasTool = hasTool,
      hasWorkplace = hasWorkplace)
  for i in knownRecipes.len .. recipesIndexes.high:
    if recipesTable.row == gameSettings.listsLimit + 1 or i > studies.high:
      break
    if recipeName.len > 0 and ("Study " & itemsList[recipesIndexes[
        i].parseInt].name).toLowerAscii.find(sub = recipeName.toLowerAscii,
        start = 1) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    if (showType == 2 and not canCraft) or (showType == 3 and canCraft):
      continue
    addButton(table = recipesTable, text = "Study " & itemsList[recipesIndexes[
        i].parseInt].name, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, column = 1)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasWorkplace, column = 2)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasTool, column = 3, newRow = true)
  for i in (knownRecipes.len + studies.len) .. recipesIndexes.high:
    if recipesTable.row == gameSettings.listsLimit + 1:
      break
    if recipeName.len > 0 and ("Deconstruct " & itemsList[recipesIndexes[
        i].parseInt].name).toLowerAscii.find(sub = recipeName.toLowerAscii,
        start = 1) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    if showType == 3:
      continue
    addButton(table = recipesTable, text = "Deconstruct " & itemsList[
        recipesIndexes[i].parseInt].name, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Deconstruct " & $recipesIndexes[i] & "} " &
        $canCraft, column = 1)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Deconstruct " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasWorkplace, column = 2)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Deconstruct " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasTool, column = 3, newRow = true)
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  if page > 1:
    if recipesTable.row < gameSettings.listsLimit + 1:
      addPagination(table = recipesTable, previousCommand = "ShowCrafting " & $(
          page - 1) & (if recipeName.len > 0: " {" & recipeName & "}" else: ""),
          nextCommand = "")
    else:
      addPagination(table = recipesTable, previousCommand = "ShowCrafting " & $(
          page - 1) & (if recipeName.len > 0: " {" & recipeName & "}" else: ""),
          nextCommand = "ShowCrafting " & $(page + 1) & (if recipeName.len >
          0: " {" & recipeName & "}" else: ""))
  elif recipesTable.row == gameSettings.listsLimit + 1:
    addPagination(table = recipesTable, previousCommand = "",
        nextCommand = "ShowCrafting " & $(page + 1) & (if recipeName.len >
        0: " {" & recipeName & "}" else: ""))
  updateTable(table = recipesTable, grabFocus = not (tclEval2(
      script = "focus") == searchEntry))
  craftsFrame = craftsCanvas & ".craft"
  tclEval(script = craftsCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = craftsCanvas & " create window 0 0 -anchor nw -window " & craftsFrame)
  tclEval(script = "update")
  tclEval(script = craftsCanvas & " configure -scrollregion [list " & tclEval2(
      script = craftsCanvas & " bbox all") & "]")
  showScreen(newScreenName = "craftframe")
  tclSetResult(value = "1")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowCrafting", showCraftingCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc checkAdaTool(toolNeeded: cstring): int {.sideEffect, raises: [], tags: [], exportc.} =
  if checkTool(toolNeeded = $toolNeeded):
    return 1
  return 0

proc isAdaCraftable(adaRecipe: AdaCraftData; canCraft, hasWorkplace, hasTool,
    hasMaterials: var cint) {.sideEffect, raises: [], tags: [], exportc.} =
  var
    materials: seq[string]
    amounts: seq[Positive]
  for material in adaRecipe.materialTypes:
    if material.len > 0:
      materials.add(y = $material)
  for amount in adaRecipe.materialAmounts:
    if amount > 0:
      amounts.add(y = amount)
  let recipe = CraftData(workplace: adaRecipe.workplace.ModuleType,
      tool: $adaRecipe.tool, materialTypes: materials, materialAmounts: amounts)
  var cCraft, hWorkplace, hTool, hMaterials: bool = false
  try:
    isCraftable(recipe = recipe, canCraft = cCraft, hasWorkplace = hWorkplace,
        hasTool = hTool, hasMaterials = hMaterials)
  except:
    echo getCurrentExceptionMsg()
  canCraft = (if cCraft: 1 else: 0)
  hasWorkplace = (if hWorkplace: 1 else: 0)
  hasTool = (if hTool: 1 else: 0)
  hasMaterials = (if hMaterials: 1 else: 0)

proc checkAdaStudyPrerequisities(canCraft, hasTool,
    hasWorkplace: var cint) {.sideEffect, raises: [], tags: [], exportc.} =
  var cCraft, hTool, hWorkplace: bool = false
  try:
    checkStudyPrerequisities(canCraft = cCraft, hasTool = hTool,
        hasWorkplace = hWorkplace)
  except:
    echo getCurrentExceptionMsg()
  canCraft = (if cCraft: 1 else: 0)
  hasWorkplace = (if hWorkplace: 1 else: 0)
  hasTool = (if hTool: 1 else: 0)
