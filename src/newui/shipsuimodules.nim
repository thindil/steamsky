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

## Provides code related to the information about the player's ship's modules
## members, like listing them, showing information, changing their settings,
## etc.

import std/[algorithm, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crafts, game, types]
import coreui, dialogs, errordialog, setui, table, themes

type ModulesSortOrders = enum
  nameAsc, nameDesc, damageAsc, damageDesc, infoAsc, infoDesc, none

const defaultModulesSortOrder: ModulesSortOrders = none

var modulesSortOrder: ModulesSortOrders = defaultModulesSortOrder

proc getModuleInfo(moduleIndex: Natural): string {.raises: [],
    tags: [], contractual.} =
  ## Get the additional information about the module
  ##
  ## * moduleIndex - the index of the module in the player's ship to show info
  ##
  ## Returns the string with the additional information about the module or the
  ## empty string if no info is available
  let module: ModuleData = playerShip.modules[moduleIndex]
  case module.mType
  of gun:
    try:
      if module.ammoIndex in 0..playerShip.cargo.high and itemsList[
          playerShip.cargo[module.ammoIndex].protoIndex].itemType ==
          itemsTypesList[modulesList[module.protoIndex].value - 1]:
        result = "Uses " & itemsList[playerShip.cargo[
            module.ammoIndex].protoIndex].name & ", "
      else:
        result = "No ammunition assigned, "
    except:
      result = "No ammunition assigned, "
    if module.owner[0] == -1:
      result.add(y = " no gunner.")
    else:
      result.add(y = " " & playerShip.crew[module.owner[0]].name & " is gunner.")
  of workshop:
    let recipeName: string = try:
        getWorkshopRecipeName(workshop = moduleIndex)
      except:
        ""
    if recipeName.len > 0:
      result = recipeName
      var hasWorkers: bool = false
      for owner in module.owner:
        if owner > -1:
          if hasWorkers:
            result.add(y = ", " & playerShip.crew[owner].name)
          else:
            result.add(y = ", workers: " & playerShip.crew[owner].name)
          hasWorkers = true
      if not hasWorkers:
        result.add(y = ", no workers assigned")
      result.add(y = ".")
    else:
      result = "No crafting order."
  of engine:
    if module.disabled:
      result = "Engine disabled"
  of trainingRoom:
    try:
      if module.trainedSkill > 0:
        result = "Set for training " & skillsList[module.trainedSkill].name
        var hasTrainees: bool = false
        for owner in module.owner:
          if owner > -1:
            if hasTrainees:
              result.add(y = ", " & playerShip.crew[owner].name)
            else:
              result.add(y = ", trainees: " & playerShip.crew[owner].name)
            hasTrainees = true
        if not hasTrainees:
          result.add(y = ", no trainees assigned")
        result.add(y = ".")
      else:
        result = "Not set for training."
    except:
      result = "Not set for training."
  else:
    discard

proc sortModules(sortAsc, sortDesc: ModulesSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort the modules on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if modulesSortOrder == sortAsc:
    modulesSortOrder = sortDesc
  else:
    modulesSortOrder = sortAsc
  type LocalModuleData = object
    name: string
    damage: float
    id: Natural
    info: string
  var localModules: seq[LocalModuleData] = @[]
  for index, module in playerShip.modules:
    localModules.add(y = LocalModuleData(name: module.name, damage: (
        module.durability / module.maxDurability).float, id: index,
        info: getModuleInfo(moduleIndex = index)))

  proc sortModules(x, y: LocalModuleData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two modules and return which should go first, based on the sort
    ## order of the modules
    ##
    ## * x - the first member to compare
    ## * y - the second member to compare
    ##
    ## Returns 1 if the first module should go first, -1 if the second module
    ## should go first.
    case modulesSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      return -1
    of damageAsc:
      if x.damage < y.damage:
        return 1
      return -1
    of damageDesc:
      if x.damage > y.damage:
        return 1
      return -1
    of infoAsc:
      if x.info < y.info:
        return 1
      return -1
    of infoDesc:
      if x.info > y.info:
        return 1
      return -1
    of none:
      return -1

  localModules.sort(cmp = sortModules)
  modulesIndexes = @[]
  for module in localModules:
    modulesIndexes.add(y = module.id)

var moduleIndex*: Natural = 0 ## The index of currently selected module

proc setModuleInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the dialog with information about the selected module
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  moduleIndex = data
  dialog = moduleInfoDialog
  setDialog(x = windowWidth / 10, y = windowHeight / 10)

proc showModuleDamage(module: ModuleData) {.raises: [], tags: [], contractual.} =
  ## Show information about the selected module's damage
  ##
  ## * module - the currently selected module
  setLayoutRowDynamic(height = 35, cols = 4, ratio = [0.4.cfloat, 0.44, 0.08, 0.08])
  label(str = "Status:")
  let
    damagePercent: float = (module.durability.float / module.maxDurability.float)
    statusTooltip: string =  if damagePercent < 1.0 and damagePercent > 0.79:
        "Slightly damaged"
      elif damagePercent < 0.8 and damagePercent > 0.49:
        "Damaged"
      elif damagePercent < 0.5 and damagePercent > 0.19:
        "Heavily damaged"
      elif damagePercent < 0.2 and damagePercent > 0.0:
        "Almost destroyed"
      elif damagePercent == 0.0:
        "Destroyed"
      else:
        "Not damaged"

proc showModuleInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog with information about the selected module
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  const
    width: float = 600
    height: float = 500

  let
    module: ModuleData = playerShip.modules[moduleIndex]
    windowName: string = module.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
    label(str = "Name:")
    colorLabel(str = module.name, color = theme.colors[goldenColor])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Set a new name for the module")
    imageButton(image = images[editIcon]):
      dialog = renameModuleDialog
    showModuleDamage(module = module)
    setLayoutRowDynamic(height = 30, cols = 1)
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

const
  headers: array[3, HeaderData[ModulesSortOrders]] = [
    HeaderData[ModulesSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[ModulesSortOrders](label: "Durability", sortAsc: damageAsc,
        sortDesc: damageDesc),
    HeaderData[ModulesSortOrders](label: "Additional info", sortAsc: infoAsc,
        sortDesc: infoDesc)]
  ratio: array[3, cfloat] = [300.cfloat, 200, 500]

proc showModulesInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the player's ship's modules
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Show the list of modules
  addHeader(headers = headers, ratio = ratio, tooltip = "modules",
      code = sortModules, dialog = dialog)
  var currentRow: Positive = 1
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
  let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
  var row: Positive = 1
  for index in modulesIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    addButton(label = playerShip.modules[index].name,
        tooltip = "Show the module's info", data = index, code = setModuleInfo,
        dialog = dialog)
    addProgressBar(tooltip = "Show the module's info",
        value = playerShip.modules[index].durability,
        maxValue = playerShip.modules[index].maxDurability, data = index,
        code = setModuleInfo, dialog = dialog)
    addButton(label = getModuleInfo(moduleIndex = index),
        tooltip = "Show the module's info", data = index, code = setModuleInfo,
        dialog = dialog)
    row.inc
    if row == gameSettings.listsLimit + 1:
      break
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
