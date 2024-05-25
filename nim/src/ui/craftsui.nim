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

import std/[algorithm, os, strutils, tables]
import ../[config, crafts, crewinventory, game, shipscrew, tk, types]
import coreui, dialogs, table, updateheader, utilsui2

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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Show information about available crafting recipes
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCrafting page recipename
  ## Page is the current page of recipes list to show, recipename is the
  ## text which will be searching in the recipes names. Can be empty, then
  ## show all recipes.
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
    showType = try:
        tclEval2(script = typeBox & " current").parseInt + 1
      except:
        return showError(message = "Can't get the show type value.")
    page = try:
        (if argc == 2: ($argv[1]).parseInt else: 1)
      except:
        return showError(message = "Can't get the page.")
    startRow = (page - 1) * gameSettings.listsLimit + 1
  var
    currentRow = 1
    canCraft, hasWorkplace, hasTool, hasMaterials = false
  for index, rec in recipesIndexes:
    if index > knownRecipes.high:
      break
    try:
      if recipeName.len > 0 and itemsList[recipesList[
          rec].resultIndex].name.toLowerAscii.find(
          sub = recipeName.toLowerAscii,
          start = 1) == -1:
        continue
    except:
      return showError(message = "Can't check recipeName.")
    if currentRow < startRow:
      currentRow.inc
      continue
    let recipe = try:
        recipesList[rec]
      except:
        return showError(message = "Can't get the recipe.")
    isCraftable(recipe = recipe, canCraft = canCraft,
        hasWorkplace = hasWorkplace, hasTool = hasTool,
        hasMaterials = hasMaterials)
    if (showType == 2 and not canCraft) or (showType == 3 and canCraft):
      continue
    try:
      addButton(table = recipesTable, text = itemsList[recipe.resultIndex].name,
          tooltip = "Show recipe's details", command = "ShowRecipeInfo {" &
              rec &
          "} " & $canCraft, column = 1)
    except:
      return showError(message = "Can't add the button.")
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
    try:
      if recipeName.len > 0 and ("Study " & itemsList[recipesIndexes[
          i].parseInt].name).toLowerAscii.find(sub = recipeName.toLowerAscii,
          start = 1) == -1:
        continue
    except:
      return showError(message = "Can't check the recipeName in study.")
    if currentRow < startRow:
      currentRow.inc
      continue
    if (showType == 2 and not canCraft) or (showType == 3 and canCraft):
      continue
    try:
      addButton(table = recipesTable, text = "Study " & itemsList[
          recipesIndexes[i].parseInt].name, tooltip = "Show recipe's details",
          command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
          $canCraft, column = 1)
    except:
      return showError(message = "Can't add button in study.")
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasWorkplace, column = 2)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasTool, column = 3, newRow = true)
  for i in (knownRecipes.len + studies.len) .. recipesIndexes.high:
    if recipesTable.row == gameSettings.listsLimit + 1:
      break
    try:
      if recipeName.len > 0 and ("Deconstruct " & itemsList[recipesIndexes[
          i].parseInt].name).toLowerAscii.find(sub = recipeName.toLowerAscii,
          start = 1) == -1:
        continue
    except:
      return showError(message = "Can't check recipeName in deconstruct.")
    if currentRow < startRow:
      currentRow.inc
      continue
    if showType == 3:
      continue
    try:
      addButton(table = recipesTable, text = "Deconstruct " & itemsList[
          recipesIndexes[i].parseInt].name, tooltip = "Show recipe's details",
          command = "ShowRecipeInfo {Deconstruct " & $recipesIndexes[i] & "} " &
          $canCraft, column = 1)
    except:
      return showError(message = "Can't add button in deconstruct.")
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

type RecipesSortOrders = enum
  nameAsc, nameDesc, workplaceAsc, workplaceDesc, toolsAsc, toolsDesc,
    materialsAsc, materialsDesc, none

const defaultRecipesSortOrder = none

var recipesSortOrder = defaultRecipesSortOrder

proc sortCraftingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Sort the list of crafting recipes
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortCrafting x
  ## X is X axis coordinate where the player clicked the mouse button
  let column = try:
      getColumnNumber(table = recipesTable, xPosition = ($argv[1]).parseInt)
    except:
      return showError(message = "Can't get the column.")
  case column
  of 1:
    if recipesSortOrder == nameAsc:
      recipesSortOrder = nameDesc
    else:
      recipesSortOrder = nameAsc
  of 2:
    if recipesSortOrder == workplaceAsc:
      recipesSortOrder = workplaceDesc
    else:
      recipesSortOrder = workplaceAsc
  of 3:
    if recipesSortOrder == toolsAsc:
      recipesSortOrder = toolsDesc
    else:
      recipesSortOrder = toolsAsc
  of 4:
    if recipesSortOrder == materialsAsc:
      recipesSortOrder = materialsDesc
    else:
      recipesSortOrder = materialsAsc
  else:
    discard
  if recipesSortOrder == none:
    return tclOk
  type LocalModuleData = object
    name: string
    workplace: bool
    tool: bool
    materials: bool
    id: string
  var
    localRecipes: seq[LocalModuleData]
    canCraft, hasWorkplace, hasTool, hasMaterials = false
  for recipe in knownRecipes:
    try:
      isCraftable(recipe = recipesList[recipe], canCraft = canCraft,
          hasWorkplace = hasWorkplace, hasTool = hasTool,
          hasMaterials = hasMaterials)
      localRecipes.add(y = LocalModuleData(name: itemsList[recipesList[
          recipe].resultIndex].name, workplace: hasWorkplace, tool: hasTool,
          materials: hasMaterials, id: recipe))
    except:
      return showError(message = "Can't sort known recipes.")
  proc sortRecipes(x, y: LocalModuleData): int =
    case recipesSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      else:
        return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      else:
        return -1
    of workplaceAsc:
      if x.workplace < y.workplace:
        return 1
      else:
        return -1
    of workplaceDesc:
      if x.workplace > y.workplace:
        return 1
      else:
        return -1
    of toolsAsc:
      if x.tool < y.tool:
        return 1
      else:
        return -1
    of toolsDesc:
      if x.tool > y.tool:
        return 1
      else:
        return -1
    of materialsAsc:
      if x.materials < y.materials:
        return 1
      else:
        return -1
    of materialsDesc:
      if x.materials > y.materials:
        return 1
      else:
        return -1
    of none:
      return -1
  localRecipes.sort(cmp = sortRecipes)
  recipesIndexes = @[]
  for recipe in localRecipes:
    recipesIndexes.add(y = recipe.id)
  checkStudyPrerequisities(canCraft = canCraft, hasTool = hasTool,
      hasWorkplace = hasWorkplace)
  localRecipes = @[]
  for recipe in studies:
    try:
      localRecipes.add(y = LocalModuleData(name: itemsList[recipe].name,
          workplace: hasWorkplace, tool: hasTool, materials: true, id: $recipe))
    except:
      return showError(message = "Can't sort studies.")
  localRecipes.sort(cmp = sortRecipes)
  for recipe in localRecipes:
    recipesIndexes.add(y = recipe.id)
  localRecipes = @[]
  for recipe in deconstructs:
    try:
      localRecipes.add(y = LocalModuleData(name: itemsList[recipe].name,
          workplace: hasWorkplace, tool: hasTool, materials: true, id: $recipe))
    except:
      return showError(message = "Can't sort deconstructs.")
  localRecipes.sort(cmp = sortRecipes)
  for recipe in localRecipes:
    recipesIndexes.add(y = recipe.id)
  return showCraftingCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowCrafting", "1"].allocCStringArray)

proc showSetRecipeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show dialog to set the selected recipe as crafting order
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetRecipe index
  ## Index is the index of the recipe to craft.
  let
    recipeIndex = $argv[1]
    recipeLength = recipeIndex.len
    recipeType = (if recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Study": "Study" elif recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Decon": "Deconstruct" else: "Craft")
    craftDialog = try:
        createDialog(name = ".craftdialog", title = recipeType & " " &
          (if recipeType == "Study": itemsList[recipeIndex[6 ..
          ^1].parseInt].name elif recipeType == "Deconstruct": itemsList[
          recipeIndex[12 .. ^1].parseInt].name else: itemsList[
          recipeIndex.parseInt].name), titleWidth = 275, columns = 2)
      except:
        return showError(message = "Can't create a dialog.")
    maxAmount = try:
        checkRecipe(recipeIndex = recipeIndex)
      except:
        return showError(message = "Can't get max amount.")
    amountBox = craftDialog & ".amount"
  tclEval(script = "ttk::spinbox " & amountBox & " -from 1 -to " & $maxAmount &
      " -validate key -validatecommand {ValidateSpinbox %W %P " & craftDialog & ".craft} -width 20")
  tclEval(script = amountBox & " set 1")
  tclSetVar(varName = "craftworker", newValue = "noone")
  var label = craftDialog & ".amountlabel"
  tclEval(script = "ttk::label " & label & " -text {Amount:}")
  var button = craftDialog & ".maxamount"
  tclEval(script = "ttk::button " & button & " -text {max " & $maxAmount &
      "} -command {" & amountBox & " set " & $maxAmount & ";" & amountBox & " validate}")
  var
    firstFocus = ""
    buttonRow = 1
  if recipeType != "Study":
    if maxAmount > 1:
      tclEval(script = "grid " & label)
      tclEval(script = "grid " & button & " -row 1 -column 1 -padx {0 5}")
      tclEval(script = "tooltip::tooltip " & button & " \"Set maximum possible amount of how many times\\nthe crafting order should be done.\"")
      tclEval(script = "bind " & button & " <Tab> {focus " & amountBox & ";break}")
      tclEval(script = "bind " & button & " <Escape> {" & craftDialog & ".cancel invoke;break}")
      firstFocus = ".maxamount"
    else:
      tclEval(script = "grid " & label & " -columnspan 2")
    tclEval(script = "grid " & amountBox & " -columnspan 2 -padx 5")
    tclEval(script = "tooltip::tooltip " & amountBox & " \"Set amount of how many times the crafting order\\nshould be done.\"")
    tclEval(script = "bind " & amountBox & " <Tab> {focus " & craftDialog & ".noworker;break}")
    tclEval(script = "bind " & amountBox & " <Escape> {" & craftDialog & ".cancel invoke;break}")
    if firstFocus.len == 0:
      firstFocus = ".amount"
    buttonRow = buttonRow + 2
  var mType: ModuleType
  if recipeType in ["Study", "Deconstruct"]:
    mType = alchemyLab
  else:
    mType = try:
        recipesList[recipeIndex].workplace
      except:
        return showError(message = "Can't get a module's type.")
  var
    modulesList2 = ""
    modulesAmount = 0
  for module in playerShip.modules:
    try:
      if modulesList[module.protoIndex].mType == mType:
        modulesList2.add(y = " {" & module.name & "}")
        modulesAmount.inc
    except:
      return showError(message = "Can't create the list of modules.")
  let modulesBox = craftDialog & ".workshop"
  tclEval(script = "ttk::combobox " & modulesBox & " -state readonly")
  tclEval(script = modulesBox & " configure -values [list" & modulesList2 & "]")
  tclEval(script = modulesBox & " current 0")
  if modulesAmount > 1:
    label = craftDialog & ".workshoplabel"
    tclEval(script = "ttk::label " & label & " -text {Workshop:}")
    tclEval(script = "grid " & label & " -columnspan 2 -padx 5")
    tclEval(script = "grid " & modulesBox & " -columnspan 2 -padx 5")
    tclEval(script = "bind " & modulesBox & " <Escape> {" & craftDialog & ".cancel invoke;break}")
    buttonRow = buttonRow + 2
    if firstFocus.len == 0:
      firstFocus = ".workshop"
  var crafterButton = craftDialog & ".noworker"
  tclEval(script = "ttk::radiobutton " & crafterButton & " -text {Don't assing anyone} -variable craftworker -value noone")
  tclEval(script = "grid " & crafterButton & " -columnspan 2 -padx 5 -sticky w")
  tclEval(script = "tooltip::tooltip " & crafterButton & " \"Don't assign anyone to the order. You can\\nmanually do it later, in ship info screen.\"")
  tclEval(script = "bind " & crafterButton & " <Tab> {focus " & craftDialog & ".bestworker;break}")
  tclEval(script = "bind " & crafterButton & " <Escape> {" & craftDialog & ".cancel invoke;break}")
  if firstFocus.len == 0:
    firstFocus = ".noworker"
  crafterButton = craftDialog & ".bestworker"
  tclEval(script = "ttk::radiobutton " & crafterButton & " -text {Assign the best worker} -variable craftworker -value best")
  tclEval(script = "grid " & crafterButton & " -columnspan 2 -padx 5 -sticky w")
  tclEval(script = "tooltip::tooltip " & crafterButton & " \"Assign the crew member with the highest skill\\nneeded for the recipe, even if the crew member\\nis busy.\"")
  tclEval(script = "bind " & crafterButton & " <Escape> {" & craftDialog & ".cancel invoke;break}")
  crafterButton = craftDialog & ".selectedworker"
  tclEval(script = "ttk::radiobutton " & crafterButton & " -text {Assign selected member} -variable craftworker -value fromlist")
  tclEval(script = "grid " & crafterButton & " -columnspan 2 -padx 5 -sticky w")
  tclEval(script = "tooltip::tooltip " & crafterButton & " \"Assign the crew member from the list.\\nThe sign + after name means that this crew member has\\nneeded skill, the sign ++ after name means that his/her\\nneeded skill is the best in the crew.\"")
  let crewBox = craftDialog & ".members"
  tclEval(script = "ttk::combobox " & crewBox & " -state readonly")
  tclEval(script = "bind " & crafterButton & " <Tab> {focus " & crewBox & ";break}")
  tclEval(script = "bind " & crafterButton & " <Escape> {" & craftDialog & ".cancel invoke;break}")
  var crewList = ""
  let recipe = try:
      setRecipeData(recipeIndex = recipeIndex)
    except:
      return showError(message = "Can't get the recipe.")
  for index, member in playerShip.crew:
    crewList.add(y = " {" & member.name & getSkillMarks(
        skillIndex = recipe.skill, memberIndex = index) & "}")
  tclEval(script = crewBox & " configure -values [list" & crewList & "]")
  tclEval(script = crewBox & " current 0")
  tclEval(script = "grid " & crewBox & " -columnspan 2 -padx 5")
  tclEval(script = "tooltip::tooltip " & crewBox & " \"Assign the crew member from the list.\\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\\nneeded skill is the best in the crew.\"")
  tclEval(script = "bind " & crewBox & " <Tab> {focus " & craftDialog & ".craft;break}")
  tclEval(script = "bind " & crewBox & " <Escape> {" & craftDialog & ".cancel invoke;break}")
  buttonRow = buttonRow + 4
  button = craftDialog & ".craft"
  tclEval(script = "ttk::button " & button & " -text {" & recipeType &
      "} -command {SetCrafting {" & $argv[1] & "};CloseDialog " & craftDialog &
      "} -image " & recipeType.toLowerAscii & "2icon -style Dialoggreen.TButton")
  tclEval(script = "grid " & button & " -pady 5 -padx 5")
  tclEval(script = "tooltip::tooltip " & button & " \"Set the crafting order.\"")
  tclEval(script = "bind " & button & " <Escape> {" & craftDialog & ".cancel invoke;break}")
  button = craftDialog & ".cancel"
  tclEval(script = "ttk::button " & button &
      " -text {Cancel} -command {CloseDialog " & craftDialog & "} -image cancelicon -style Dialogred.TButton")
  tclEval(script = "grid " & button & " -pady 5 -padx 5 -column 1 -row " & $buttonRow)
  tclEval(script = "tooltip::tooltip " & button & " \"Cancel setting the order and close dialog. \\[Escape key\\]\"")
  tclEval(script = "bind " & button & " <Tab> {focus " & craftDialog &
      firstFocus & ";break}")
  tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")
  showDialog(dialog = craftDialog)
  tclEval(script = "focus " & button)
  return tclOk

proc setCraftingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Set the selected recipe as a crafting order in the selected workshop
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetCrafting index
  ## Index is the index of the crafting recipe to set
  var recipeIndex = $argv[1]
  if recipeIndex[0] == '{':
    recipeIndex = recipeIndex[1 .. ^2]
  let
    modulesBox = ".craftdialog.workshop"
    amountBox = ".craftdialog.amount"
    assignWorker = tclGetVar(varName = "craftworker")
    memberBox = ".craftdialog.members"
  var workshopIndex = try:
      tclEval2(script = modulesBox & " current").parseInt + 1
    except:
      return showError(message = "Can't get the workshop index.")
  for index, module in playerShip.modules:
    if module.name == tclEval2(script = modulesBox & " get"):
      workshopIndex.dec
    if workshopIndex == 0:
      try:
        setRecipe(workshop = index, amount = tclEval2(script = amountBox &
            " get").parseInt, recipeIndex = recipeIndex)
      except:
        return showError(message = "Can't set the recipe.")
      if assignWorker == "fromlist":
        try:
          giveOrders(ship = playerShip, memberIndex = tclEval2(
              script = memberBox & " current").parseInt, givenOrder = craft,
              moduleIndex = index)
        except:
          return showError(message = "Can't give order from list.")
      elif assignWorker == "best":
        let recipe = try:
            setRecipeData(recipeIndex = recipeIndex)
          except:
            return showError(message = "Can't set the recipe's data.")
        var workerAssigned = false
        for mIndex, member in playerShip.crew:
          if getSkillMarks(skillIndex = recipe.skill, memberIndex = mIndex) == " ++":
            try:
              giveOrders(ship = playerShip, memberIndex = mIndex,
                  givenOrder = craft, moduleIndex = index)
            except:
              return showError(message = "Can't give order to best worker.")
            workerAssigned = true
            break
        if not workerAssigned:
          try:
            giveOrders(ship = playerShip, memberIndex = 0, givenOrder = craft,
                moduleIndex = index)
          except:
            return showError(message = "Can't give order to the player.")
      updateHeader()
      updateMessages()
      break
  return tclOk

proc showRecipeInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let
    recipeIndex = $argv[1]
    recipeLength = recipeIndex.len
    recipeType = (if recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Study": "Study" elif recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Decon": "Deconstruct" else: "Craft")
    recipeDialog = createDialog(name = ".recipedialog", title = (
        if recipeType == "Study": "Study " & itemsList[recipeIndex[6 ..
        ^1].parseInt].name elif recipeType == "Deconstruct": "Deconstruct " &
        itemsList[recipeIndex[12 .. ^1].parseInt].name else: "Craft " &
        itemsList[recipesList[recipeIndex].resultIndex].name), titleWidth = 275)
    recipeText = recipeDialog & ".text"
  tclEval(script = "text " & recipeText & " -wrap char -height 15 -width 40")
  tclEval(script = recipeText & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowCrafting", showCraftingCommand)
#    addCommand("SortCrafting", sortCraftingCommand)
#    addCommand("ShowSetRecipe", showSetRecipeCommand)
#    addCommand("SetCrafting", setCraftingCommand)
#    addCommand("ShowRecipeInfo", showRecipeInfoCommand)
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
