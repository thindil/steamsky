# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides code related to crafting items, like showing the list of available
## recipes, starting crafting, etc.

import std/[algorithm, math, strutils, tables]
import contracts, nimalyzer
import ../[config, crafts, crewinventory, game, items, shipmodules, shipscrew, tk, types]
import coreui, dialogs, errordialog, table, updateheader, utilsui2

proc checkTool(toolNeeded: string): bool {.raises: [], tags: [], contractual.} =
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
        let cargoIndex: int = findItem(inventory = playerShip.cargo,
            protoIndex = index, itemQuality = any)
        if cargoIndex > -1:
          result = true
          break

proc isCraftable(recipe: CraftData; canCraft, hasWorkplace, hasTool,
    hasMaterials: var bool) {.raises: [], tags: [WriteIOEffect, TimeEffect,
        RootEffect], contractual.} =
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
        var cargoIndex: int = findItem(inventory = playerShip.cargo,
            protoIndex = itemIndex, itemQuality = any)
        if cargoIndex > -1 and playerShip.cargo[cargoIndex].amount >=
            recipe.materialAmounts[materialIndex]:
          hasMaterials = true
  if hasTool and hasMaterials and hasWorkplace:
    canCraft = true

proc checkStudyPrerequisities(canCraft, hasTool,
    hasWorkplace: var bool) {.raises: [], tags: [WriteIOEffect, TimeEffect,
        RootEffect], contractual.} =
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

{.push ruleOff: "varDeclared".}
var
  studies: seq[Positive] = @[]
  deconstructs: seq[Positive] = @[]
  recipesIndexes: seq[string] = @[]
  recipesTable, ordersTable: TableWidget
{.pop ruleOn: "varDeclared".}

proc showWorkshopsTable(craftsCanvas, craftsFrame: string): bool {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the list of workshops with their crafting orders
  ##
  ## * craftsCanvas - the Tcl canvas in which the table will be added
  ## * craftsFrame  - the Tcl frame in which the table will be added
  ##
  ## Return true if there was an error, otherwise false
  if ordersTable.rowHeight == 0:
    ordersTable = createTable(parent = craftsCanvas & ".craft.orders",
        headers = @["Workshop", "Order", "Workers"],
        scrollbar = craftsFrame & ".scrolly", command = "SortCrafting2",
        tooltipText = "Press mouse button to sort the workshops.")
  else:
    ordersTable.clearTable
  for index, module in playerShip.modules:
    if module.mType != ModuleType2.workshop:
      continue
    var
      recipeName2: string = try:
          getWorkshopRecipeName(workshop = index)
      except:
        showError(message = "Can't get the recipe name.")
        return true
      tooltipText: string = "Cancel the selected order"
      command: string = "ChangeCraftOrder " & $index & " cancel"
    if recipeName2.len == 0:
      recipeName2 = "Not set"
      tooltipText = "Set a new order for the workshop"
      command = "ChangeCraftOrder " & $index & " new"
    addButton(table = ordersTable, text = module.name, tooltip = tooltipText,
        command = command, column = 1)
    addButton(table = ordersTable, text = recipeName2, tooltip = tooltipText,
        command = command, column = 2)
    var workers: string = ""
    var haveWorkers: bool = false
    for worker in module.owner:
      if worker > -1:
        if haveWorkers:
          workers.add(y = ", ")
        haveWorkers = true
        workers.add(y = playerShip.crew[worker].name)
    if not haveWorkers:
      workers = "none"
    addButton(table = ordersTable, text = workers, tooltip = tooltipText,
        command = command, column = 3, newRow = true)
  updateTable(table = ordersTable)
  return false

proc showRecipesTable(craftsCanvas, craftsFrame, recipeName,
    searchEntry: string; argc: cint; argv: cstringArray): bool {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the list of crafting recipes known by the player
  ##
  ## * craftsCanvas - the Tcl canvas in which the table will be added
  ## * craftsFrame  - the Tcl frame in which the table will be added
  ## * recipeName   - the text to search in the names of recipes
  ## * searchEntry  - the Tcl name of search field
  ## * argc         - the amount of arguments entered for the command
  ## * argv         - the list of the command's arguments
  ##
  ## Return true if there was an error, otherwise false
  if recipesTable.rowHeight == 0:
    recipesTable = createTable(parent = craftsCanvas & ".craft.recipes",
        headers = @["Name", "Workshop", "Tools", "Materials"],
            scrollbar = craftsFrame &
        ".scrolly", command = "SortCrafting",
        tooltipText = "Press mouse button to sort the crafting recipes.")
    tclEval(script = "grid configure " & recipesTable.canvas & " -row 1")
  else:
    recipesTable.clearTable
  let
    typeBox: string = craftsCanvas & ".craft.recipes.sframe.show"
    showType: Natural = try:
        tclEval2(script = typeBox & " current").parseInt + 1
      except:
        showError(message = "Can't get the show type value.")
        return true
    page: Positive = try:
        (if argc == 2: ($argv[1]).parseInt else: 1)
      except:
        showError(message = "Can't get the page.")
        return true
    startRow: Natural = (page - 1) * gameSettings.listsLimit + 1
  var
    currentRow: Positive = 1
    canCraft, hasWorkplace, hasTool, hasMaterials: bool = false
  let workshop: int = try:
        tclGetVar(varName = "workshop").parseInt
      except:
        showError(message = "Can't get workshop index")
        return true
  for index, rec in recipesIndexes:
    if index > knownRecipes.high:
      break
    try:
      if recipeName.len > 0 and itemsList[recipesList[
          rec].resultIndex].name.toLowerAscii.find(
          sub = recipeName.toLowerAscii) == -1:
        continue
    except:
      showError(message = "Can't check recipeName.")
      return true
    let recipe: CraftData = try:
        recipesList[rec]
      except:
        showError(message = "Can't get the recipe.")
        return true
    try:
      if workshop > -1 and modulesList[playerShip.modules[
          workshop].protoIndex].mType != recipe.workplace:
        continue
    except:
      showError(message = "Can't check the workshop")
      return true
    if currentRow < startRow:
      currentRow.inc
      continue
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
      showError(message = "Can't add the button.")
      return true
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
  for i in knownRecipes.len..recipesIndexes.high:
    if recipesTable.row == gameSettings.listsLimit + 1 or i > studies.high:
      break
    try:
      if recipeName.len > 0 and ("Study " & itemsList[recipesIndexes[
          i].parseInt].name).toLowerAscii.find(sub = recipeName.toLowerAscii,
          start = 1) == -1:
        continue
    except:
      showError(message = "Can't check the recipeName in study.")
      return true
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
      showError(message = "Can't add button in study.")
      return true
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasWorkplace, column = 2)
    addCheckButton(table = recipesTable, tooltip = "Show recipe's details",
        command = "ShowRecipeInfo {Study " & $recipesIndexes[i] & "} " &
        $canCraft, checked = hasTool, column = 3, newRow = true)
  for i in (knownRecipes.len + studies.len)..recipesIndexes.high:
    if recipesTable.row == gameSettings.listsLimit + 1:
      break
    try:
      if recipeName.len > 0 and ("Deconstruct " & itemsList[recipesIndexes[
          i].parseInt].name).toLowerAscii.find(sub = recipeName.toLowerAscii,
          start = 1) == -1:
        continue
    except:
      showError(message = "Can't check recipeName in deconstruct.")
      return true
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
      showError(message = "Can't add button in deconstruct.")
      return true
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
  return false

proc showCraftingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], cdecl, contractual, ruleOff: "params".} =
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
  var craftsFrame: string = mainPaned & ".craftframe"
  let craftsCanvas: string = craftsFrame & ".canvas"
  if tclEval2(script = "winfo exists " & craftsCanvas) == "0":
    tclEval(script = """
      ttk::frame .gameframe.paned.craftframe
      set craftcanvas [canvas .gameframe.paned.craftframe.canvas \
         -yscrollcommand [list .gameframe.paned.craftframe.scrolly set] \
         -xscrollcommand [list .gameframe.paned.craftframe.scrollx set]]
      set workshop -1
      pack [ttk::scrollbar .gameframe.paned.craftframe.scrolly -orient vertical \
         -command [list $craftcanvas yview]] -side right -fill y
      pack $craftcanvas -side top -fill both
      pack [ttk::scrollbar .gameframe.paned.craftframe.scrollx -orient horizontal \
         -command [list $craftcanvas xview]] -fill x
      ::autoscroll::autoscroll .gameframe.paned.craftframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.craftframe.scrollx
      set craftframe [ttk::frame $craftcanvas.craft]
      set newtab recipes
      grid [ttk::frame $craftframe.tabs]
      grid [ttk::radiobutton $craftframe.tabs.recipes -text {Recipes} \
         -state selected -style Radio.Toolbutton -value recipes -variable newtab \
         -command ShowCraftingTab] -padx 5
      grid [ttk::radiobutton $craftframe.tabs.orders -text {Workshops} \
         -style Radio.Toolbutton -value orders -variable newtab \
         -command ShowCraftingTab] -row 0 -column 1 -padx 5
      set craftframe2 [ttk::frame $craftframe.recipes]
      ttk::frame $craftframe2.sframe
      grid [ttk::label $craftframe2.sframe.searchlabel -text {Name:}]
      tooltip::tooltip $craftframe2.sframe.searchlabel \
         {Search for the selected recipe.}
      grid [ttk::entry $craftframe2.sframe.search -validate key \
         -validatecommand {ShowCrafting 1 %P} -width 30] -sticky w -row 0 -column 1
      tooltip::tooltip $craftframe2.sframe.search {Search for the selected recipe.}
      grid [ttk::label $craftframe2.sframe.showlabel -text {Show:}]
      tooltip::tooltip $craftframe2.sframe.showlabel \
         {Show only the selected type of recipes.}
      grid [ttk::combobox $craftframe2.sframe.show \
         -values [list {All} {Craftable only} {Non-craftable only}] -width 15 -state readonly] \
         -sticky w -row 1 -column 1
      tooltip::tooltip $craftframe2.sframe.show {Show only the selected type of recipes.}
      $craftframe2.sframe.show current 0
      bind $craftframe2.sframe.show <<ComboboxSelected>> {ShowCrafting 1}
      grid [ttk::label $craftframe2.sframe.workshoplabel -text {Workshop:}]
      tooltip::tooltip $craftframe2.sframe.workshoplabel \
         {Show only recipes craftable in the selected workshop.}
      grid [ttk::combobox $craftframe2.sframe.workshop \
         -values [list {All}] -width 29 -state readonly] \
         -sticky w -row 2 -column 1
      tooltip::tooltip $craftframe2.sframe.workshop \
         {Show only recipes craftable in the selected workshop.}
      $craftframe2.sframe.workshop current 0
      bind $craftframe2.sframe.workshop <<ComboboxSelected>> {SetCraftWorkshop}
      ttk::frame $craftframe.orders
      grid [ttk::label $craftframe.orders.orders]
      SetScrollbarBindings $craftcanvas .gameframe.paned.craftframe.scrolly
      SetScrollbarBindings $craftframe .gameframe.paned.craftframe.scrolly
    """)
    tclEval(script = "bind " & craftsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & craftsCanvas) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    tclEval(script = "grid remove " & gameHeader & ".morebutton")
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "crafts")
  tclEval(script = gameHeader & ".morebutton configure -command {CraftsMore show}")
  var workshops: string = "{All}"
  for module in playerShip.modules:
    if module.mType == workshop:
      workshops.add(y = " {" & module.name & "}")
  tclEval(script = craftsCanvas & ".craft.recipes.sframe.workshop configure -values [list " &
      workshops & "]")
  let
    recipeName: string = (if argc == 3: $argv[2] else: "")
    searchEntry: string = craftsCanvas & ".craft.recipes.sframe.search"
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
  if showRecipesTable(craftsCanvas = craftsCanvas, craftsFrame = craftsFrame,
      recipeName = recipeName, searchEntry = searchEntry, argc = argc, argv = argv):
    return tclOk
  if showWorkshopsTable(craftsCanvas = craftsCanvas, craftsFrame = craftsFrame):
    return tclOk
  if tclGetVar(varName = "newtab") == "recipes":
    tclEval(script = "grid " & gameHeader & ".morebutton -row 0 -column 2")
  else:
    tclEval(script = "grid remove " & gameHeader & ".morebutton")
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
  tclEval(script = "ShowCraftingTab show")
  tclSetResult(value = "1")
  return tclOk

type RecipesSortOrders = enum
  nameAsc, nameDesc, workplaceAsc, workplaceDesc, toolsAsc, toolsDesc,
    materialsAsc, materialsDesc, none

const defaultRecipesSortOrder: RecipesSortOrders = none

var recipesSortOrder: RecipesSortOrders = defaultRecipesSortOrder

proc sortCraftingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
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
  let column: int = try:
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
    localRecipes: seq[LocalModuleData] = @[]
    canCraft, hasWorkplace, hasTool, hasMaterials: bool = false
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

  proc sortRecipes(x, y: LocalModuleData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two recipes and return which should go first, based on the sort
    ## order of the recipes
    ##
    ## * x - the first recipe to compare
    ## * y - the second recipe to compare
    ##
    ## Returns 1 if the first recipe should go first, -1 if the second recipe
    ## should go first.
    case recipesSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      return -1
    of workplaceAsc:
      if x.workplace < y.workplace:
        return 1
      return -1
    of workplaceDesc:
      if x.workplace > y.workplace:
        return 1
      return -1
    of toolsAsc:
      if x.tool < y.tool:
        return 1
      return -1
    of toolsDesc:
      if x.tool > y.tool:
        return 1
      return -1
    of materialsAsc:
      if x.materials < y.materials:
        return 1
      return -1
    of materialsDesc:
      if x.materials > y.materials:
        return 1
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
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
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
    recipeIndex: string = $argv[1]
    recipeLength: Natural = recipeIndex.len
    recipeType: string = (if recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Study": "Study" elif recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Decon": "Deconstruct" else: "Craft")
    craftDialog: string = try:
        createDialog(name = ".craftdialog", title = recipeType & " " &
          (if recipeType == "Study": itemsList[recipeIndex[6 ..
          ^1].parseInt].name elif recipeType == "Deconstruct": itemsList[
          recipeIndex[12 .. ^1].parseInt].name else: itemsList[
          recipeIndex.parseInt].name), titleWidth = 275, columns = 2)
      except:
        return showError(message = "Can't create a dialog.")
    maxAmount: int = try:
        checkRecipe(recipeIndex = recipeIndex)
      except ValueError:
        return showError(message = "Can't get max amount.")
      except CraftingNoWorkshopError:
        showMessage(text = "You can't start crafting because you don't have a workshop.",
            title = "Can't start crafting")
        return tclOk
      except CraftingNoMaterialsError:
        showMessage(text = "You can't start crafting because you don't have materials.",
            title = "Can't start crafting")
        return tclOk
      except CraftingNoToolsError:
        showMessage(text = "You can't start crafting because you don't have a proper tool.",
            title = "Can't start crafting")
        return tclOk
      except TradeNoFreeCargoError:
        showMessage(text = "You can't start crafting because you don't have free cargo.",
            title = "Can't start crafting")
        return tclOk
    amountBox: string = craftDialog & ".amount"
  tclEval(script = "ttk::spinbox " & amountBox & " -from 1 -to " & $maxAmount &
      " -validate key -validatecommand {ValidateSpinbox %W %P " & craftDialog & ".craft} -width 20")
  tclEval(script = amountBox & " set 1")
  tclSetVar(varName = "craftworker", newValue = "noone")
  var label: string = craftDialog & ".amountlabel"
  tclEval(script = "ttk::label " & label & " -text {Amount:}")
  var button: string = craftDialog & ".maxamount"
  tclEval(script = "ttk::button " & button & " -text {max " & $maxAmount &
      "} -command {" & amountBox & " set " & $maxAmount & ";" & amountBox & " validate}")
  var
    firstFocus: string = ""
    buttonRow: Positive = 1
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
    buttonRow += 2
  var mType: ModuleType = ModuleType.any
  if recipeType in ["Study", "Deconstruct"]:
    mType = alchemyLab
  else:
    mType = try:
        recipesList[recipeIndex].workplace
      except:
        return showError(message = "Can't get a module's type.")
  var
    modulesList2: string = ""
    modulesAmount: Natural = 0
    selected: Natural = 0
    workshop: int = try:
        tclGetVar(varName = "workshop").parseInt
      except:
        -1
  for index, module in playerShip.modules:
    try:
      if modulesList[module.protoIndex].mType == mType:
        modulesList2.add(y = " {" & module.name & "}")
        if index == workshop:
          selected = modulesAmount
        modulesAmount.inc
    except:
      return showError(message = "Can't create the list of modules.")
  let modulesBox: string = craftDialog & ".workshop"
  tclEval(script = "ttk::combobox " & modulesBox & " -state readonly")
  tclEval(script = modulesBox & " configure -values [list" & modulesList2 & "]")
  tclEval(script = modulesBox & " current " & $selected)
  if modulesAmount > 1:
    label = craftDialog & ".workshoplabel"
    tclEval(script = "ttk::label " & label & " -text {Workshop:}")
    tclEval(script = "grid " & label & " -columnspan 2 -padx 5")
    tclEval(script = "grid " & modulesBox & " -columnspan 2 -padx 5")
    tclEval(script = "bind " & modulesBox & " <Escape> {" & craftDialog & ".cancel invoke;break}")
    buttonRow += 2
    if firstFocus.len == 0:
      firstFocus = ".workshop"
  var crafterButton: string = craftDialog & ".noworker"
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
  let crewBox: string = craftDialog & ".members"
  tclEval(script = "ttk::combobox " & crewBox & " -state readonly")
  tclEval(script = "bind " & crafterButton & " <Tab> {focus " & crewBox & ";break}")
  tclEval(script = "bind " & crafterButton & " <Escape> {" & craftDialog & ".cancel invoke;break}")
  var crewList: string = ""
  let recipe: CraftData = try:
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
  buttonRow += 4
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
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
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
  var recipeIndex: string = $argv[1]
  if recipeIndex[0] == '{':
    recipeIndex = recipeIndex[1..^2]
  const
    modulesBox: string = ".craftdialog.workshop"
    amountBox: string = ".craftdialog.amount"
    memberBox: string = ".craftdialog.members"
  let
    assignWorker: string = tclGetVar(varName = "craftworker")
  var workshopIndex: Natural = try:
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
        let recipe: CraftData = try:
            setRecipeData(recipeIndex = recipeIndex)
          except:
            return showError(message = "Can't set the recipe's data.")
        var workerAssigned: bool = false
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
      tclSetVar(varName = "workshop", newValue = "-1")
      break
  return tclOk

proc showRecipeInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show information about the selected recipe
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowRecipeInfo index cancraft
  ## Index is the index of the crafting recipe to show, cancraft if TRUE
  ## then recipe can be crafted (show craft button)
  let
    recipeIndex: string = $argv[1]
    recipeLength: Natural = recipeIndex.len
    recipeType: string = (if recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Study": "Study" elif recipeLength > 6 and recipeIndex[0 .. 4] ==
        "Decon": "Deconstruct" else: "Craft")
    recipeDialog: string = try:
        createDialog(name = ".recipedialog", title = (
          if recipeType == "Study": "Study " & itemsList[recipeIndex[6 ..
          ^1].parseInt].name elif recipeType == "Deconstruct": "Deconstruct " &
          itemsList[recipeIndex[12 .. ^1].parseInt].name else: "Craft " &
          itemsList[recipesList[recipeIndex].resultIndex].name),
              titleWidth = 275)
      except:
        return showError(message = "Can't create the dialog.")
    recipeText: string = recipeDialog & ".text"
  tclEval(script = "text " & recipeText & " -wrap char -height 15 -width 40")
  tclEval(script = recipeText & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  tclEval(script = recipeText & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  var recipe: CraftData = CraftData()
  if recipeType == "Study":
    try:
      recipe.materialTypes.add(y = itemsList[recipeIndex[
          6..^1].parseInt].itemType)
    except:
      return showError(message = "Can't add material.")
    recipe.resultIndex = try:
        recipeIndex[6 .. ^1].parseInt
      except:
        return showError(message = "Can't set result index.")
    recipe.materialAmounts.add(y = 1)
    recipe.resultAmount = 0
    recipe.workplace = alchemyLab
    for rec in recipesList.values:
      if rec.resultIndex == recipe.resultIndex:
        recipe.skill = rec.skill
        recipe.time = rec.difficulty * 15
        break
    recipe.difficulty = 1
    recipe.tool = alchemyTools
    recipe.toolQuality = 100
  elif recipeType == "Deconstruct":
    try:
      recipe.materialTypes.add(y = itemsList[recipeIndex[
          12..^1].parseInt].itemType)
    except:
      return showError(message = "Can't add deconstruct material.")
    recipe.resultIndex = try:
        recipeIndex[12 .. ^1].parseInt
      except:
        return showError(message = "Can't set deconstruct result")
    recipe.materialAmounts.add(y = 1)
    recipe.resultAmount = 0
    recipe.workplace = alchemyLab
    for rec in recipesList.values:
      if rec.resultIndex == recipe.resultIndex:
        recipe.skill = rec.skill
        recipe.time = rec.difficulty * 15
        recipe.difficulty = rec.difficulty
        recipe.resultIndex = findProtoItem(itemType = rec.materialTypes[0])
        recipe.resultAmount = (rec.materialAmounts[0].float * 0.8).ceil.Natural
        break
    recipe.tool = alchemyTools
    recipe.toolQuality = 100
  else:
    recipe = try:
        recipesList[recipeIndex]
      except:
        return showError(message = "Can't set recipe.")
    tclEval(script = recipeText & " insert end {Amount: }")
    tclEval(script = recipeText & " insert end {" & $recipe.resultAmount & "\n} [list gold]")
  tclEval(script = recipeText & " insert end {Materials needed: }")
  for mIndex, material in recipe.materialTypes:
    tclEval(script = recipeText & " insert end {\n- } [list gold]")
    var mAmount: int = 0
    for iIndex, item in itemsList:
      var isMaterial: bool = false
      if recipeIndex.len > 6 and recipeIndex[0..4] == "Study":
        try:
          if item.name == itemsList[recipe.resultIndex].name:
            isMaterial = true
        except:
          return showError(message = "Can't check study material.")
      elif recipeIndex.len > 12 and recipeIndex[0..10] == "Deconstruct":
        try:
          if iIndex == recipeIndex[12 .. ^1].parseInt:
            isMaterial = true
        except:
          return showError(message = "Can't check deconstruct materials.")
      else:
        if item.itemType == material:
          isMaterial = true
      if isMaterial:
        if mAmount > 0:
          tclEval(script = recipeText & " insert end { or } [list gold]")
        let cargoIndex: int = findItem(inventory = playerShip.cargo,
            protoIndex = iIndex, itemQuality = any)
        if cargoIndex > -1 and playerShip.cargo[cargoIndex].amount >=
            recipe.materialAmounts[mIndex]:
          tclEval(script = recipeText & " insert end {" &
              $recipe.materialAmounts[mIndex] & "x" & item.name & "(owned: " &
              $playerShip.cargo[cargoIndex].amount & ")} [list gold]")
        else:
          tclEval(script = recipeText & " insert end {" &
              $recipe.materialAmounts[mIndex] & "x" & item.name & "} [list red]")
        mAmount.inc
  var haveTool: bool = false
  if recipe.tool == "None":
    haveTool = true
  else:
    tclEval(script = recipeText & " insert end {\nTool: }")
    var mAmount: int = 0
    for iIndex, item in itemsList:
      haveTool = false
      if item.itemType == recipe.tool and item.value[1] <= recipe.toolQuality:
        if mAmount > 0:
          tclEval(script = recipeText & " insert end { or } [list gold]")
        let cargoIndex: int = findItem(inventory = playerShip.cargo,
            protoIndex = iIndex, quality = recipe.toolQuality,
            itemQuality = any)
        if cargoIndex > -1:
          haveTool = true
        tclEval(script = recipeText & " insert end {" & item.name & "}" & (
            if haveTool: " [list gold]" else: " [list red]"))
        mAmount.inc
  tclEval(script = recipeText & " insert end {\nWorkplace: }")
  var
    haveWorkplace: bool = false
    workplaceName: string = ""
  for module in playerShip.modules:
    try:
      if modulesList[module.protoIndex].mType == recipe.workplace:
        workplaceName = module.name
        if module.durability > 0:
          haveWorkplace = true
          break
    except:
      return showError(message = "Can't check workplace.")
  if workplaceName.len == 0:
    for index, module in modulesList:
      if module.mType == recipe.workplace:
        try:
          workplaceName = getModuleType(moduleIndex = index)
        except:
          return showError(message = "Can't get workplace name.")
        break
  tclEval(script = recipeText & " insert end {" & workplaceName & "}" & (
      if haveWorkplace: " [list gold]" else: " [list red]"))
  tclEval(script = recipeText & " insert end {\nSkill: }")
  try:
    tclEval(script = recipeText & " insert end {" & skillsList[
        recipe.skill].name & "/" & attributesList[skillsList[
        recipe.skill].attribute].name & "} [list gold]")
  except:
    return showError(message = "Can't show recipe skill.")
  if recipeType == "Craft":
    tclEval(script = recipeText & " insert end {\nDifficulty: }")
  tclEval(script = recipeText & " insert end {\nTime needed: }")
  tclEval(script = recipeText & " insert end {" & $recipe.time & " minutes} [list gold]")
  tclEval(script = recipeText & " configure -state disabled")
  tclEval(script = "grid " & recipeText & " -padx 5")
  if argv[2] == "true":
    let buttonBox: string = recipeDialog & ".buttons"
    tclEval(script = "ttk::frame " & buttonBox)
    var button: string = buttonBox & ".craft"
    tclEval(script = "ttk::button " & button & " -image " &
        recipeType.toLowerAscii & "icon -command {ShowSetRecipe {" & $argv[1] &
        "};CloseDialog " & recipeDialog & "} -style Dialog.TButton -text {" &
        recipeType & "}")
    tclEval(script = "grid " & button)
    tclEval(script = "tooltip::tooltip " & button & " \"Set crafting order (" &
        recipeType & ").\"")
    tclEval(script = "bind " & button & " <Escape> {" & buttonBox & ".close invoke;break}")
    button = buttonBox & ".close"
    tclEval(script = "ttk::button " & button &
        " -image exiticon -command {CloseDialog " & recipeDialog & "} -style Dialog.TButton -text Close")
    tclEval(script = "grid " & button & " -row 0 -column 1 -padx {5 0}")
    tclEval(script = "tooltip::tooltip " & button & " \"Close dialog \\[Escape key\\]\"")
    tclEval(script = "focus " & button)
    tclEval(script = "bind " & button & " <Tab> {focus " & buttonBox & ".craft;break}")
    tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")
    tclEval(script = "grid " & buttonBox & " -pady 5")
  else:
    addCloseButton(name = recipeDialog & ".close", text = "Close",
        command = "CloseDialog " & recipeDialog, row = 2, icon = "exiticon")
  showDialog(dialog = recipeDialog, relativeX = 0.2, relativeY = 0.1)
  return tclOk

proc showCraftingTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
  ## Show the list of known recipes or information about workshops
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCraftingTab
  let
    craftCanvas: string = mainPaned & ".craftframe.canvas"
    craftFrame: string = craftCanvas & ".craft"
  if tclGetVar(varName = "newtab") == "recipes":
    var frame: string = craftFrame & ".orders"
    tclEval(script = "grid remove " & frame)
    frame = craftFrame & ".recipes"
    tclEval(script = "grid " & frame)
  else:
    var frame: string = craftFrame & ".recipes"
    tclEval(script = "grid remove " & frame)
    frame = craftFrame & ".orders"
    tclEval(script = "grid " & frame)
  tclEval(script = craftCanvas & " delete all")
  tclEval(script = craftCanvas & " create window 0 0 -anchor nw -window " & craftFrame)
  tclEval(script = "update")
  tclEval(script = craftCanvas & " configure -scrollregion [list " &
      tclEval2(script = craftCanvas & " bbox all") & "]")
  tclSetResult(value = "1")
  if argc == 1:
    return showCraftingCommand(clientData = clientData, interp = interp,
        argc = 2, argv = @["ShowCrafting", "1"].allocCStringArray)
  return tclOk

proc changeCraftOrderCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl,
        contractual.} =
  ## Change the crafting order of the selected workshop
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ChangeCraftOrder
  let workshop: Natural = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't get the workshop index.")
  # Cancel the order
  if argv[2] == "cancel":
    try:
      cancelCraftOrder(moduleIndex = workshop)
      updateMessages()
      updateHeader()
    except CrewOrderError, CrewNoSpaceError:
      showMessage(text = getCurrentExceptionMsg(),
          title = "Can't cancel the order")
      return tclOk
    except:
      return showError(message = "Can't cancel the order.")
  else:
    if playerShip.modules[workshop].durability == 0:
      showMessage(text = "Can't set a new order because the workshop is destroyed.",
          title = "Can't set an order")
      return tclOk
    tclSetVar(varName = "workshop", newValue = $argv[1])
    tclSetVar(varName = "newtab", newValue = "recipes")
  tclEval(script = "ShowCraftingTab")
  return tclOk

proc setCraftWorkshopCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl,
        contractual.} =
  ## Set the current workshop index, based on the list of workshops
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetCraftWorkshop
  let
    workshopBox: string = mainPaned & ".craftframe.canvas.craft.recipes.sframe.workshop"
    workshop: Natural = try:
      tclEval2(script = workshopBox & " current").parseInt
    except:
      return showError(message = "Can't get workshop index.")
  if workshop == 0:
    tclSetVar(varName = "workshop", newValue = "-1")
  else:
    var workshopIndex: Natural = try:
        tclEval2(script = workshopBox & " current").parseInt
      except:
        return showError(message = "Can't get the workshop index.")
    for index, module in playerShip.modules:
      if module.name == tclEval2(script = workshopBox & " get"):
        workshopIndex.dec
      if workshopIndex == 0:
        tclSetVar(varName = "workshop", newValue = $index)
        break
  tclEval(script = "ShowCraftingTab")
  return tclOk

proc craftsMoreCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Maximize or minimize the options for the list of recipes.
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## CraftMore show/hide
  ## If th argument is set to show, show the options, otherwise hide them.
  let
    craftFrame: string = mainPaned & ".craftframe"
    button: string = gameHeader & ".morebutton"
  if argv[1] == "show":
    tclEval(script = "grid " & craftFrame & ".canvas.craft.recipes.sframe -sticky w -row 0")
    tclEval(script = button & " configure -command {CraftsMore hide}")
  else:
    tclEval(script = "grid remove " & craftFrame & ".canvas.craft.recipes.sframe")
    tclEval(script = button & " configure -command {CraftsMore show}")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand(name = "ShowCrafting", nimProc = showCraftingCommand)
    addCommand(name = "SortCrafting", nimProc = sortCraftingCommand)
    addCommand(name = "ShowSetRecipe", nimProc = showSetRecipeCommand)
    addCommand(name = "SetCrafting", nimProc = setCraftingCommand)
    addCommand(name = "ShowRecipeInfo", nimProc = showRecipeInfoCommand)
    addCommand(name = "ShowCraftingTab", nimProc = showCraftingTabCommand)
    addCommand(name = "ChangeCraftOrder", nimProc = changeCraftOrderCommand)
    addCommand(name = "SetCraftWorkshop", nimProc = setCraftWorkshopCommand)
    addCommand(name = "CraftsMore", nimProc = craftsMoreCommand)
  except:
    showError(message = "Can't add a Tcl command.")
