# Copyright 2025 Bartek thindil Jasicki
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

import std/[algorithm, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crafts, crewinventory, game, types]
import coreui, dialogs, errordialog, header, messagesui, setui, table, themes

type RecipesSortOrders = enum
  nameAsc, nameDesc, workplaceAsc, workplaceDesc, toolsAsc, toolsDesc,
    materialsAsc, materialsDesc, none

const defaultRecipesSortOrder: RecipesSortOrders = none

var
  recipesSortOrder: RecipesSortOrders = defaultRecipesSortOrder
  hasOptions: bool = true
  recipe: RecipeData = RecipeData()

proc showRecipeInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected recipe information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 400

  let
    windowName: string = recipe.name
    craft: CraftData = try:
        setRecipeData(recipeIndex = recipe.index)
      except:
        dialog = setError(message = "Can't get recipe info")
        return
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    if not recipe.index.startsWith(prefix = "Study") and
        not recipe.index.startsWith(prefix = "Deconstruct"):
      setLayoutRowDynamic(height = 30, cols = 2)
      label(str = "Amount:")
      colorLabel(str = $craft.resultAmount, color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 30, cols = 1)
    label(str = "Materials needed:")
    setLayoutRowDynamic(height = 75, cols = 1)
    group(title = "materialInfo", flags = {windowNoFlags}):
      setLayoutRowDynamic(height = 30, cols = 1)
      for mIndex, material in craft.materialTypes:
        colorLabel(str = $(mIndex + 1) & ":", color = theme.colors[goldenColor])
        for iIndex, item in itemsList:
          var isMaterial: bool = false
          if recipe.index.startsWith(prefix = "Study"):
            try:
              if item.name == itemsList[craft.resultIndex].name:
                isMaterial = true
            except:
              dialog = setError(message = "Can't check study material.")
              return
          elif recipe.index.startsWith(prefix = "Deconstruct"):
            try:
              if iIndex == recipe.index[12 .. ^1].parseInt:
                isMaterial = true
            except:
              dialog = setError(message = "Can't check deconstruct materials.")
              return
          else:
            if item.itemType == material:
              isMaterial = true
          if isMaterial:
            let cargoIndex: int = findItem(inventory = playerShip.cargo,
                protoIndex = iIndex, itemQuality = any)
            if cargoIndex > -1 and playerShip.cargo[cargoIndex].amount >=
                craft.materialAmounts[mIndex]:
              colorLabel(str = $craft.materialAmounts[mIndex] & "x" &
                  item.name & "(owned: " & $playerShip.cargo[
                  cargoIndex].amount & ")", color = theme.colors[goldenColor])
            else:
              colorLabel(str = $craft.materialAmounts[mIndex] & "x" & item.name,
                  color = theme.colors[redColor])
    var haveTool: bool = false
    if craft.tool == "None":
      haveTool = true
    else:
      setLayoutRowDynamic(height = 30, cols = 1)
      label(str = "Tool needed:")
      setLayoutRowDynamic(height = 75, cols = 1)
      group(title = "toolInfo", flags = {windowNoFlags}):
        setLayoutRowDynamic(height = 30, cols = 1)
        for iIndex, item in itemsList:
          haveTool = false
          if item.itemType == craft.tool and item.value[1] <= craft.toolQuality:
            let cargoIndex: int = findItem(inventory = playerShip.cargo,
                protoIndex = iIndex, quality = craft.toolQuality,
                itemQuality = any)
            if cargoIndex > -1:
              haveTool = true
            colorLabel(str = $item.name, color = theme.colors[
                if haveTool: goldenColor else: redColor])
    setLayoutRowDynamic(height = 30, cols = 1)
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

proc setRecipeInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the selected recipe information
  ##
  ## * data   - the index of the selected recipe
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  recipe = availableRecipes[data]
  dialog = recipeDialog

proc sortRecipes(sortAsc, sortDesc: RecipesSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort recipes on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if recipesSortOrder == sortAsc:
    recipesSortOrder = sortDesc
  else:
    recipesSortOrder = sortAsc

  proc sortRecipes(x, y: RecipeData): int {.raises: [], tags: [],
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
      if x.tools < y.tools:
        return 1
      return -1
    of toolsDesc:
      if x.tools > y.tools:
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

  availableRecipes.sort(cmp = sortRecipes)
  dialog = none

const
  headers: array[4, HeaderData[RecipesSortOrders]] = [
    HeaderData[RecipesSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[RecipesSortOrders](label: "Workshop", sortAsc: workplaceAsc,
        sortDesc: workplaceDesc),
    HeaderData[RecipesSortOrders](label: "Tools", sortAsc: toolsAsc,
        sortDesc: toolsDesc),
    HeaderData[RecipesSortOrders](label: "Materials", sortAsc: materialsAsc,
        sortDesc: materialsDesc)]
  ratio: array[4, cfloat] = [400.cfloat, 100, 100, 100]

proc showCrafting*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show information about available crafting recipes
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state,
    options = hasOptions):
    return
  # Show tab buttons
  changeStyle(field = spacing, x = 0, y = 0):
    changeStyle(field = buttonRounding, value = 0):
      setLayoutRowDynamic(height = 30, cols = 2)
      const tabs: array[2, string] = ["Recipes", "Workshops"]
      for index, tab in tabs:
        try:
          if currentTab == index:
            changeStyle(src = active, dest = normal):
              labelButton(title = tab):
                discard
          else:
            labelButton(title = tab):
              currentTab = index.cint
              if index == 0:
                hasOptions = true
              else:
                hasOptions = false
        except:
          dialog = setError(message = "Can't set the tabs buttons.")
  # Show advanced options if needed
  if showOptions and hasOptions:
    setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.2.cfloat, 0.6])
    label(str = "Name:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Search for the selected recipe.")
    editString(text = nameSearch, maxLen = 64)
    label(str = "Show:")
    const recipesTypes: array[3, string] = ["All", "Craftable only", "Non-craftable only"]
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only the selected type of recipes.")
    let newType: Natural = comboList(items = recipesTypes, selected = typeIndex,
        itemHeight = 25, x = 200, y = 150)
    if newType != typeIndex:
      typeIndex = newType
    label(str = "Workshop:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only recipes for the selected type of workshops.")
    let newWorkshop: Natural = comboList(items = workshopsList,
        selected = workshopType, itemHeight = 25, x = 400, y = 150)
    if newWorkshop != workshopType:
      workshopType = newWorkshop
  # Show the list of recipes to craft
  let tableHeight: float = windowHeight - 140 - (if showOptions: 135 else: 0) -
      gameSettings.messagesPosition.float
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "RecipesGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    addHeader(headers = headers, ratio = ratio, tooltip = "recipes",
      code = sortRecipes, dialog = dialog)
    var
      currentRow, row: Positive = 1
    let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
    saveButtonStyle()
    setButtonStyle(field = borderColor, a = 0)
    try:
      setButtonStyle(field = normal, color = theme.colors[tableRowColor])
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    except:
      dialog = setError(message = "Can't set table color")
      return
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    for index, rec in availableRecipes:
      if nameSearch.len > 0 and rec.name.toLowerAscii.find(
          sub = nameSearch.toLowerAscii) == -1:
        continue
      if typeIndex == 1 and not rec.craftable:
        continue
      if typeIndex == 2 and rec.craftable:
        continue
      try:
        if workshopIndex > 0 and rec.workshop != modulesList[playerShip.modules[
            workshopIndex - 1].protoIndex].mType:
          continue
      except:
        dialog = setError(message = "Can't check workshop.")
        return
      if currentRow < startRow:
        currentRow.inc
        continue
      addButton(label = rec.name, tooltip = "Show recipe's details.",
        data = index, code = setRecipeInfo, dialog = dialog)
      var checked: bool = rec.workplace
      addCheckButton(tooltip = "", checked = checked)
      checked = rec.tools
      addCheckButton(tooltip = "", checked = checked)
      checked = rec.materials
      addCheckButton(tooltip = "", checked = checked)
      row.inc
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      tableHeight - 20)
