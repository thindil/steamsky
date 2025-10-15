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
import ../[config, crafts, crewinventory, game, messages, missions, shipscrew,
    shipsupgrade, types]
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

proc addUpgradeButton(upgradeType: ShipUpgrade; buttonTooltip: string;
    module: ModuleData; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Add button related to upgrading the mdule
  ##
  ## * upgradeType   - the type of the upgrade to start after clicking the
  ##                   button
  ## * buttonTooltip - the tooltip to show on the button
  ## * module        - the currently selected module
  ## * dialog        - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if module.upgradeAction == upgradeType and playerShip.upgradeModule == moduleIndex:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Stop upgrading the " & buttonTooltip)
    imageButton(image = images[cancelIcon]):
      try:
        stopUpgrade()
      except CrewOrderError:
        dialog = setMessage(message = getCurrentExceptionMsg(),
            title = "Can't give orders")
      except:
        dialog = setError(message = "Can't give orders to a crew member.")
      dialog = none
  else:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Start upgrading the " & buttonTooltip)
    imageButton(image = images[upgradeButtonIcon]):
      dialog = none
      let upgradeNumber: Positive = case upgradeType
        of maxValue:
          2
        of value:
          3
        else:
          1
      try:
        startUpgrading(moduleIndex = moduleIndex, upgradeType = upgradeNumber)
      except:
        dialog = setError(message = "Can't set upgrade for the module.")
        return
      try:
        updateOrders(ship = playerShip)
      except:
        dialog = setError(message = "Can't update crew orders.")

proc showModuleDamage(module: ModuleData; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show information about the selected module's damage
  ##
  ## * module - the currently selected module
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  let moduleMaxValue: Positive = try:
      (modulesList[module.protoIndex].durability.float * 1.5).Positive
    except:
      dialog = setError(message = "Can't count the module's max value.")
      return
  if module.maxDurability < moduleMaxValue:
    setLayoutRowDynamic(height = 35, cols = 4, ratio = [0.4.cfloat, 0.44, 0.08, 0.08])
  else:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
  label(str = "Status:")
  let damagePercent: float = (module.durability.float /
        module.maxDurability.float)
  var statusTooltip: string = if damagePercent < 1.0 and damagePercent > 0.79:
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
  if module.maxDurability == moduleMaxValue:
    statusTooltip.add(y = " (max upgrade)")
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = statusTooltip)
  var value: int = module.durability
  if damagePercent < 0.8 and damagePercent > 0.19:
    changeStyle(field = progressbar, color = theme.colors[yellowColor]):
      progressBar(value = value, maxValue = module.maxDurability,
          modifyable = false)
  elif damagePercent < 0.2:
    changeStyle(field = progressbar, color = theme.colors[redColor]):
      progressBar(value = value, maxValue = module.maxDurability,
          modifyable = false)
  else:
    progressBar(value = value, maxValue = module.maxDurability,
        modifyable = false)
  if playerShip.repairModule == moduleIndex:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Remove the repair priority")
    imageButton(image = images[cancelIcon]):
      playerShip.repairModule = -1
      addMessage(message = "You removed the repair's priority.",
          mType = orderMessage)
      dialog = none
  else:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Repair the selected module as first when damaged")
    imageButton(image = images[repairPriorityIcon]):
      playerShip.repairModule = moduleIndex
      addMessage(message = "You assigned " & module.name &
          " as the repair's priority.", mType = orderMessage)
      dialog = none
  if module.maxDurability < moduleMaxValue:
    addUpgradeButton(upgradeType = durability,
        buttonTooltip = "module's durability", module = module, dialog = dialog)

proc showModuleUpgrade(module: ModuleData; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show information about the current upgrade of the module
  ##
  ## * module - the currently selected module
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  var
    moduleInfo: string = ""
    maxUpgrade: Natural = 0
  case module.upgradeAction
  of durability:
    moduleInfo.add(y = "Durability")
    maxUpgrade = try:
        modulesList[module.protoIndex].durability
    except:
      dialog = setError(message = "Can't get max upgrade.")
      return
  of maxValue:
    try:
      case modulesList[module.protoIndex].mType
      of engine:
        moduleInfo.add(y = "Power")
        maxUpgrade = (modulesList[module.protoIndex].maxValue / 20).int
      of cabin:
        moduleInfo.add(y = "Quality")
        maxUpgrade = modulesList[module.protoIndex].maxValue
      of gun, batteringRam:
        moduleInfo.add(y = "Damage")
        maxUpgrade = modulesList[module.protoIndex].maxValue * 2
      of hull:
        moduleInfo.add(y = "Enlarge")
        maxUpgrade = modulesList[module.protoIndex].maxValue * 40
      of harpoonGun:
        moduleInfo.add(y = "Strength")
        maxUpgrade = modulesList[module.protoIndex].maxValue * 10
      else:
        discard
    except:
      dialog = setError(message = "Can't show info about upgrade.")
      return
  of value:
    try:
      if modulesList[module.protoIndex].mType == engine:
        moduleInfo.add(y = "Fuel usage")
        maxUpgrade = modulesList[module.protoIndex].value * 20
    except:
      dialog = setError(message = "Can't show info about fuel usage ugprade.")
      return
  else:
    discard
  maxUpgrade = (maxUpgrade.float * newGameSettings.upgradeCostBonus).int
  if maxUpgrade == 0:
    maxUpgrade = 1
  if playerShip.upgradeModule == moduleIndex:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
  else:
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
  label(str = "Upgrade progress:")
  var upgradePercent: int = 100 - ((module.upgradeProgress.float /
      maxUpgrade.float) * 100.0).int
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = moduleInfo)
  if upgradePercent > 74:
    progressBar(value = upgradePercent, maxValue = 100, modifyable = false)
  elif upgradePercent > 24:
    changeStyle(field = progressbar, color = theme.colors[yellowColor]):
      progressBar(value = upgradePercent, maxValue = 100, modifyable = false)
  else:
    changeStyle(field = progressbar, color = theme.colors[redColor]):
      progressBar(value = upgradePercent, maxValue = 100, modifyable = false)
  if playerShip.upgradeModule == moduleIndex:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Stop upgrading the module")
    imageButton(image = images[cancelIcon]):
      try:
        stopUpgrade()
      except CrewOrderError:
        dialog = setMessage(message = getCurrentExceptionMsg(),
            title = "Can't give orders")
      except:
        dialog = setError(message = "Can't give orders to a crew member.")

proc showEngineInfo(module: ModuleData; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show information about the selected engine
  ##
  ## * module - the currently selected module
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Show the engine's power
  var moduleMaxValue2: Natural = try:
      (modulesList[module.protoIndex].maxValue.float * 1.5).int
    except:
      dialog = setError(message = "Can't count the module max value.")
      return
  if module.maxDurability < moduleMaxValue2:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
  else:
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
  label(str = "Max power:")
  colorLabel(str = $module.power & (if module.power ==
      moduleMaxValue2: " (max upgrade)" else: ""), color = theme.colors[goldenColor])
  if module.power < moduleMaxValue2:
    addUpgradeButton(upgradeType = maxValue, buttonTooltip = "engine's power",
        module = module, dialog = dialog)
  # Show the engine's fuel usage
  moduleMaxValue2 = try:
        (modulesList[module.protoIndex].value.float / 2.0).int
    except:
      dialog = setError(message = "Can't count the module's max value (2).")
      return
  if module.maxDurability > moduleMaxValue2:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
  else:
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
  label(str = "Fuel usage:")
  colorLabel(str = $module.fuelUsage & (if moduleMaxValue2 ==
      module.fuelUsage: " (max upgrade)" else: ""), color = theme.colors[goldenColor])
  if module.fuelUsage > moduleMaxValue2:
    addUpgradeButton(upgradeType = value, buttonTooltip = "engine's fuel usage",
        module = module, dialog = dialog)
  # Show engine state
  setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
  label(str = "State:")
  colorLabel(str = (if module.disabled: "Disabled" else: "Enabled"),
      color = theme.colors[goldenColor])
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = "Turn " & (
        if module.disabled: "on " else: "off ") & " the engine")
  imageButton(image = images[powerIcon]):
    if playerShip.modules[moduleIndex].disabled:
      playerShip.modules[moduleIndex].disabled = false
      addMessage(message = "You enabled " & playerShip.modules[
          moduleIndex].name & ".", mType = orderMessage)
    else:
      var canDisable: bool = false
      for index, module in playerShip.modules:
        if module.mType == ModuleType2.engine and (not module.disabled and
            index != moduleIndex):
          canDisable = true
          break
      if not canDisable:
        dialog = setMessage(message = "You can't disable this engine because it is your last working engine.",
            title = "Can't disable engine")
        return
      playerShip.modules[moduleIndex].disabled = true
      addMessage(message = "You disabled " & playerShip.modules[
          moduleIndex].name & ".", mType = orderMessage)

proc addOwnersInfo(module: ModuleData; ownersName: string) {.raises: [], tags: [], contractual.} =
  ## Show information about the selected module's owners
  ##
  ## * module     - the currently selected module
  ## * ownersName - the text to display on the label for owners
  var ownersText: string = ownersName
  if module.owner.len > 1:
    ownersText.add(y = "s")
  ownersText.add(y = " (max " & $module.owner.len & "):")
  setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
  label(str = ownersText)

proc showCabinInfo(module: ModuleData; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show information about the selected cabin
  ##
  ## * module - the currently selected module
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  var isPassenger: bool = false
  block missionLoop:
    for mission in acceptedMissions:
      if mission.mType == passenger:
        for owner in module.owner:
          if mission.data == owner:
            isPassenger = true
            break missionLoop
  addOwnersInfo(module = module, ownersName = "Owners")

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
      flags = {windowBorder, windowTitle, windowMovable}):
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
    # Show the module's name
    label(str = "Name:")
    colorLabel(str = module.name, color = theme.colors[goldenColor])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Set a new name for the module")
    imageButton(image = images[editIcon]):
      dialog = renameModuleDialog
    # Show the module's status
    showModuleDamage(module = module, dialog = dialog)
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
    # Show the module's weight
    label(str = "Weight:")
    colorLabel(str = $module.weight & " kg", color = theme.colors[goldenColor])
    # Show the module's size
    label(str = "Size:")
    try:
      colorLabel(str = $modulesList[module.protoIndex].size,
          color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show the module's size")
      return
    # Show the module's repair material
    setLayoutRowDynamic(height = 35, cols = 4, ratio = [0.4.cfloat, 0.2, 0.2, 0.2])
    label(str = "Repair material:")
    var manyMaterials: bool = false
    for item in itemsList.values:
      try:
        if item.itemType == modulesList[module.protoIndex].repairMaterial:
          if manyMaterials:
            label(str = " or ")
          colorLabel(str = item.name, color = theme.colors[(if findItem(
              inventory = playerShip.cargo, itemType = item.itemType,
              itemQuality = any) == -1: redColor else: goldenColor)])
          manyMaterials = true
      except:
        dialog = setError(message = "Can't count repair material.")
        return
    # Show the module's upgrade skill
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
    label(str = "Repair skill:")
    try:
      colorLabel(str = skillsList[modulesList[
          module.protoIndex].repairSkill].name & "/" & attributesList[skillsList[
          modulesList[module.protoIndex].repairSkill].attribute].name,
          color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show repair skill.")
      return
    # Show the module's upgrade action
    if module.upgradeAction != none:
      showModuleUpgrade(module = module, dialog = dialog)
    # Show information specific to the module's type
    case module.mType
    # Show information about engine
    of engine:
      showEngineInfo(module = module, dialog = dialog)
    # Show information about cargo room
    of cargoRoom:
      setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
      label(str = "Max cargo:")
      try:
        colorLabel(str = $modulesList[module.protoIndex].maxValue & " kg",
            color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't show the max cargo.")
        return
    # Show information about hull
    of hull:
      let moduleMaxValue2: int = try:
          (modulesList[module.protoIndex].maxValue.float * 1.5).int
        except:
          dialog = setError(message = "Can't count the module's max value (3).")
          return
      if module.maxModules < moduleMaxValue2:
        setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.08])
      else:
        setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.5])
      label(str = "Modules installed:")
      colorLabel(str = $module.installedModules & " / " & $module.maxModules & (
          if module.maxModules == moduleMaxValue2: " (max upgrade)" else: ""),
          color = theme.colors[goldenColor])
      if module.maxModules < moduleMaxValue2:
        addUpgradeButton(upgradeType = maxValue,
            buttonTooltip = "hull's size so it can have more modules installed",
            module = module, dialog = dialog)
    # Show information about cabin
    of cabin:
      showCabinInfo(module = module, dialog = dialog)
    else:
      discard
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
