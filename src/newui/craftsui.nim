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
import ../[config, crafts, crewinventory, game, shipscrew, shipmodules, types]
import coreui, dialogs, errordialog, header, messagesui, setui, table, themes, utilsui2

type
  RecipesSortOrders = enum
    nameAsc, nameDesc, workplaceAsc, workplaceDesc, toolsAsc, toolsDesc,
      materialsAsc, materialsDesc, none
  AssignType = enum
    noone, best, selected
  WorkshopsSortOrders = enum
    nameAsc, nameDesc, orderAsc, orderDesc, workersAsc, workersDesc, none

const
  defaultRecipesSortOrder: RecipesSortOrders = none
  defaultWorkshopsSortOrder: WorkshopsSortOrders = none

var
  recipesSortOrder: RecipesSortOrders = defaultRecipesSortOrder
  hasOptions: bool = true
  recipe: RecipeData = RecipeData()
  craft: CraftData = CraftData()
  maxAmount, craftAmount, craftWorkshop: Natural = 0
  craftQuality: Natural = 2
  craftImage, setImage: PImage = nil
  workshops, workers, bonuses, maluses: seq[string] = @[]
  assign: AssignType = noone
  worker, bonus, malus: Natural = 0
  workshopsSortOrder: WorkshopsSortOrders = defaultWorkshopsSortOrder

proc showRecipeInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected recipe information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  const
    width: float = 400
    height: float = 500

  let windowName: string = recipe.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowMovable}):
    if recipe.recipeType == craftType:
      setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.3.cfloat, 0.7])
      label(str = "Amount:")
      colorLabel(str = $craft.resultAmount, color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 30, cols = 1)
    label(str = "Materials needed:")
    setLayoutRowDynamic(height = 85, cols = 1)
    group(title = "materialInfo", flags = {windowNoFlags}):
      setLayoutRowDynamic(height = 30, cols = 1)
      for mIndex, material in craft.materialTypes:
        colorLabel(str = $(mIndex + 1) & ":", color = theme.colors[goldenColor])
        for iIndex, item in itemsList:
          var isMaterial: bool = false
          if recipe.recipeType == study:
            try:
              if item.name == itemsList[craft.resultIndex].name:
                isMaterial = true
            except:
              dialog = setError(message = "Can't check study material.")
              return
          elif recipe.recipeType == deconstruct:
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
      setLayoutRowDynamic(height = 85, cols = 1)
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
    setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.3.cfloat, 0.7])
    label(str = "Workplace:")
    var
      haveWorkplace: bool = false
      workplaceName: string = ""
    for module in playerShip.modules:
      try:
        if modulesList[module.protoIndex].mType == craft.workplace:
          workplaceName = module.name
          if module.durability > 0:
            haveWorkplace = true
            break
      except:
        dialog = setError(message = "Can't check workplace.")
        return
    if workplaceName.len == 0:
      for index, module in modulesList:
        if module.mType == craft.workplace:
          try:
            workplaceName = getModuleType(moduleIndex = index)
          except:
            dialog = setError(message = "Can't get workplace name.")
            return
          break
    colorLabel(str = workplaceName, color = theme.colors[
        if haveWorkplace: goldenColor else: redColor])
    label(str = "Skill:")
    try:
      colorLabel(str = skillsList[craft.skill].name & "/" & attributesList[
          skillsList[craft.skill].attribute].name, color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show recipe skill.")
      return
    if recipe.recipeType == craftType:
      label(str = "Difficulty:")
      colorLabel(str = getRecipeDifficultyName(difficulty = craft.difficulty),
          color = theme.colors[goldenColor])
    label(str = "Time needed:")
    colorLabel(str = $craft.time & " minutes", color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 30, cols = (if recipe.craftable: 2 else: 1))
    if recipe.craftable:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set crafting order (" & $recipe.recipeType & ")")
      imageLabelButton(image = craftImage, text = $recipe.recipeType,
          alignment = right):
        dialog = setRecipeDialog
        craftAmount = 1
        maxAmount = try:
            checkRecipe(recipeIndex = recipe.index)
          except ValueError:
            dialog = setError(message = "Can't get max amount.")
            return
          except CraftingNoWorkshopError:
            dialog = setMessage(message = "You can't start crafting because you don't have a workshop.",
                title = "Can't start crafting")
            return
          except CraftingNoMaterialsError:
            dialog = setMessage(message = "You can't start crafting because you don't have materials.",
                title = "Can't start crafting")
            return
          except CraftingNoToolsError:
            dialog = setMessage(message = "You can't start crafting because you don't have a proper tool.",
                title = "Can't start crafting")
            return
          except TradeNoFreeCargoError:
            dialog = setMessage(message = "You can't start crafting because you don't have free cargo.",
                title = "Can't start crafting")
            return
        craftQuality = 2
        var mType: ModuleType = ModuleType.any
        if recipe.recipeType == craftType:
          mType = try:
              recipesList[recipe.index].workplace
            except:
              dialog = setError(message = "Can't get a module's type.")
              return
        else:
          mType = alchemyLab
        workshops = @[]
        for index, module in playerShip.modules:
          try:
            if modulesList[module.protoIndex].mType == mType:
              workshops.add(y = module.name)
          except:
            dialog = setError(message = "Can't create the list of modules.")
            return
        if workshopIndex > workshops.len:
          workshopIndex = 0
        assign = noone
        workers = @[]
        worker = 0
        for index, member in playerShip.crew:
          if member.skills.len > 0:
            workers.add(y = member.name & getSkillMarks(
                skillIndex = craft.skill, memberIndex = index))
        bonus = 0
        bonuses = @[]
        for bonus in CraftBonuses:
          bonuses.add(y = $bonus)
        malus = 0
        maluses = @[]
        for malus in CraftMaluses:
          maluses.add(y = $malus)
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

proc showSetRecipe*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog for setting the selected crafting recipe
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 500

  let windowName: string = recipe.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowMovable}):
    if recipe.recipeType == craftType:
      # Show amount setting
      setLayoutRowDynamic(height = 30, cols = 2)
      label(str = "Amount:")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set maximum possible amount of how many times the crafting order should be done.")
      labelButton(title = "max " & $maxAmount):
        craftAmount = maxAmount
      setLayoutRowDynamic(height = 30, cols = 1)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set amount of how many times the crafting order should be done.")
      let newAmount: int = property2(name = "#", min = 1, val = craftAmount,
          max = maxAmount, step = 1, incPerPixel = 1)
      if craftAmount != newAmount:
        craftAmount = newAmount
      # Show quality setting
      setLayoutRowDynamic(height = 30, cols = 2)
      label(str = "Quality:")
      const qualities: array[5, string] = ["Poor", "Low", "Normal", "Good", "Excellent"]
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Desired quality of crafted item. Better quality raises difficulty of crafting, worse lowers.")
      craftQuality = comboList(items = qualities, selected = craftQuality,
          itemHeight = 25, x = 200, y = 150)
      # Show special properties setting
      setLayoutRowDynamic(height = 30, cols = 1)
      label(str = "Special:")
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.2, 0.4])
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Optional special bonus for the crafted item. If you select any, you will need also to select a malus for it.")
      var newBonus: Natural = comboList(items = bonuses, selected = bonus,
          itemHeight = 25, x = 200, y = 150)
      if newBonus != bonus:
        bonus = newBonus
        malus = (if bonus > 0: 1 else: 0)
        maluses = @[]
        for malus in CraftMaluses:
          if newBonus > 0 and malus.ord == newBonus:
            continue
          maluses.add(y = $malus)
      label(str = "<=>", alignment = centered)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Optional special malus for the crafted item. If you select any, you will need also to select a bonus for it.")
      var newMalus: Natural = comboList(items = maluses, selected = malus,
          itemHeight = 25, x = 200, y = 150)
      if newMalus != malus:
        malus = newMalus
        bonus = (if malus > 0: 1 else: 0)
        bonuses = @[]
        for bonus in CraftBonuses:
          if newMalus > 0 and bonus.ord == newMalus:
            continue
          bonuses.add(y = $bonus)
    setLayoutRowDynamic(height = 30, cols = 1)
    # Show workshop setting if needed
    if workshops.len > 1:
      label(str = "Workshop:")
      craftWorkshop = comboList(items = workshops, selected = craftWorkshop,
          itemHeight = 25, x = 380, y = 150)
    # Show assign crew setting
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Don't assign anyone to the order. You can manually do it later, in ship info screen.")
    if option(label = "Don't assing anyone", selected = assign == noone):
      assign = noone
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Assign the crew member with the highest skill needed for the recipe, even if the crew member is busy.")
    if option(label = "Assign the best worker", selected = assign == best):
      assign = best
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Assign the crew member from the list. The sign + after name means that this crew member has needed skill, the sign ++ after name means that his/her needed skill is the best in the crew.")
    if option(label = "Assign selected member", selected = assign == selected):
      assign = selected
    worker = comboList(items = workers, selected = worker, itemHeight = 25,
        x = 380, y = 150)
    setLayoutRowDynamic(height = 30, cols = 2)
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    imageLabelButton(image = setImage, text = $recipe.recipeType,
        alignment = right):
      dialog = none
      let moduleName: string = workshops[craftWorkshop]
      for index, module in playerShip.modules:
        if module.name == moduleName:
          if craftWorkshop == 0:
            try:
              setRecipe(workshop = index, amount = craftAmount,
                  recipeIndex = recipe.index,
                  quality = craftQuality.ObjectQuality)
            except:
              dialog = setError(message = "Can't set the recipe.")
              return
            if assign == selected:
              try:
                giveOrders(ship = playerShip, memberIndex = worker,
                    givenOrder = CrewOrders.craft, moduleIndex = index)
              except:
                dialog = setError(message = "Can't give order from list.")
                return
            elif assign == best:
              var workerAssigned: bool = false
              for mIndex, member in playerShip.crew:
                if getSkillMarks(skillIndex = craft.skill,
                    memberIndex = mIndex) == " ++":
                  try:
                    giveOrders(ship = playerShip, memberIndex = mIndex,
                        givenOrder = CrewOrders.craft, moduleIndex = index)
                  except:
                    dialog = setError(message = "Can't give order to best worker.")
                  workerAssigned = true
                  break
              if not workerAssigned:
                try:
                  giveOrders(ship = playerShip, memberIndex = 0,
                      givenOrder = CrewOrders.craft, moduleIndex = index)
                except:
                  dialog = setError(message = "Can't give order to the player.")
            setWorkshopsList(dialog = dialog)
            break
          craftWorkshop.dec
    restoreButtonStyle()
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
  craft = try:
      setRecipeData(recipeIndex = recipe.index)
    except:
      dialog = setError(message = "Can't get crafting recipe info")
      return
  dialog = recipeDialog
  case recipe.recipeType
    of craftType:
      craftImage = images[craftIcon]
      setImage = images[craftColoredIcon]
    of study:
      craftImage = images[studyIcon]
      setImage = images[studyColoredIcon]
    of deconstruct:
      craftImage = images[deconstructIcon]
      setImage = images[deconstructColoredIcon]

proc setChangeOrder(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the selected workshop to change its crafting order or to show recipes
  ## for it
  ##
  ## * data   - the index of the selected recipe
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  let recipeName2: string = try:
        getWorkshopRecipeName(workshop = data)
      except:
        dialog = setError(message = "Can't get the recipe name.")
        return
  if recipeName2.len == 0:
    workshopIndex = data
    currentTab = 0
    hasOptions = true
  else:
    try:
      cancelCraftOrder(moduleIndex = data)
    except CrewOrderError, CrewNoSpaceError:
      dialog = setMessage(message = getCurrentExceptionMsg(),
          title = "Can't cancel the order")
    except:
      dialog = setError(message = "Can't cancel the order.")
    setWorkshopsList(dialog = dialog)

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

proc sortWorkshops(sortAsc, sortDesc: WorkshopsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort workshops on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if workshopsSortOrder == sortAsc:
    workshopsSortOrder = sortDesc
  else:
    workshopsSortOrder = sortAsc

  proc sortWorkshops(x, y: WorkshopData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two workshops and return which should go first, based on the sort
    ## order of the workshops
    ##
    ## * x - the first workshop to compare
    ## * y - the second workshop to compare
    ##
    ## Returns 1 if the first workshop should go first, -1 if the second workshop
    ## should go first.
    case workshopsSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      return -1
    of orderAsc:
      if x.order < y.order:
        return 1
      return -1
    of orderDesc:
      if x.order > y.order:
        return 1
      return -1
    of workersAsc:
      if x.workers < y.workers:
        return 1
      return -1
    of workersDesc:
      if x.workers > y.workers:
        return 1
      return -1
    of none:
      return -1

  workshopsList2.sort(cmp = sortWorkshops)
  dialog = none

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
    typeIndex = comboList(items = recipesTypes, selected = typeIndex,
        itemHeight = 25, x = 200, y = 150)
    label(str = "Workshop:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only recipes craftable in the selected workshop.")
    workshopType = comboList(items = workshopsList, selected = workshopType,
        itemHeight = 25, x = 400, y = 150)
  let tableHeight: float = windowHeight - 140 - (if showOptions: 135 else: 0) -
      gameSettings.messagesPosition.float
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "CraftingGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show the list of recipes to craft
    if currentTab == 0:

      const
        headers: array[4, HeaderData[RecipesSortOrders]] = [
          HeaderData[RecipesSortOrders](label: "Name", sortAsc: nameAsc,
              sortDesc: nameDesc),
          HeaderData[RecipesSortOrders](label: "Workshop",
              sortAsc: workplaceAsc, sortDesc: workplaceDesc),
          HeaderData[RecipesSortOrders](label: "Tools", sortAsc: toolsAsc,
              sortDesc: toolsDesc),
          HeaderData[RecipesSortOrders](label: "Materials",
              sortAsc: materialsAsc, sortDesc: materialsDesc)]
        ratio: array[4, cfloat] = [400.cfloat, 100, 100, 100]

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
          if workshopIndex > 0 and rec.workshop != modulesList[
              playerShip.modules[workshopIndex].protoIndex].mType:
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
    # Show the list of installed workshops
    else:

      const
        headers: array[3, HeaderData[WorkshopsSortOrders]] = [
          HeaderData[WorkshopsSortOrders](label: "Name", sortAsc: nameAsc,
              sortDesc: nameDesc),
          HeaderData[WorkshopsSortOrders](label: "Order", sortAsc: orderAsc,
              sortDesc: orderDesc),
          HeaderData[WorkshopsSortOrders](label: "Workers", sortAsc: workersAsc,
              sortDesc: workersDesc)]
        ratio: array[3, cfloat] = [400.cfloat, 400, 100]

      addHeader(headers = headers, ratio = ratio, tooltip = "workshops",
        code = sortWorkshops, dialog = dialog)
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
      for module in workshopsList2:
        addButton(label = module.name, tooltip = module.tooltip,
            data = module.index, code = setChangeOrder, dialog = dialog)
        addButton(label = module.order, tooltip = module.tooltip,
            data = module.index, code = setChangeOrder, dialog = dialog)
        addButton(label = module.workers, tooltip = module.tooltip,
            data = module.index, code = setChangeOrder, dialog = dialog)
      restoreButtonStyle()
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      tableHeight - 20)
