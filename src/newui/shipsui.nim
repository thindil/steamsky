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

## Provides code related to the information about the player's ship, like
## minimizing/maximizin its sections, setting the ship's name, etc.

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, maps, messages, reputation, ships, shipscrew, types]
import coreui, dialogs, errordialog, header, mapsui, messagesui, setui,
    shipsuicargo, shipsuicrew, shipsuimodules, themes

var
  expandedSection: Natural = 0
  newName: string = ""

proc showRenameDialog*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog to rename things
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 200

  let windowName: string = "Rename the " & (case dialog
    of renameDialog:
      "ship"
    of renameMemberDialog:
      "crew member"
    of renameModuleDialog:
      "module"
    else:
      "")
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 1)
    label(str = "Enter a new name" & (case dialog
      of renameDialog:
        ""
      of renameMemberDialog:
        " for " & playerShip.crew[crewIndex].name
      of renameModuleDialog:
        " for " & playerShip.modules[moduleIndex].name
      else:
        "") & ":")
    editString(text = newName, maxLen = 64)
    setLayoutRowDynamic(height = 30, cols = 2)
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    if newName.len == 0:
      disabled:
        imageLabelButton(image = images[editColoredIcon], text = "Rename",
            alignment = right):
          discard
    else:
      imageLabelButton(image = images[editColoredIcon], text = "Rename",
          alignment = right):
        case dialog
        of renameDialog:
          playerShip.name = newName
        of renameMemberDialog:
          playerShip.crew[crewIndex].name = newName
        of renameModuleDialog:
          playerShip.modules[moduleIndex].name = newName
        else:
          discard
        newName = ""
        dialog = none
    restoreButtonStyle()
    addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
        isPopup = false, label = "Cancel")

  windowSetFocus(name = windowName)

proc cancelUpgrade(dialog: var GameDialog) {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Cancel the current player's ship's upgrade
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  playerShip.upgradeModule = -1
  for index, member in playerShip.crew:
    if member.order == upgrading:
      try:
        giveOrders(ship = playerShip, memberIndex = index,
            givenOrder = rest)
      except CrewOrderError:
        dialog = setMessage(message = getCurrentExceptionMsg(),
            title = "Can't give orders")
        return
      except:
        dialog = setError(message = "Can't give orders to a crew member.")
        return
      break
  addMessage(message = "You stopped current upgrade.", mType = orderMessage)

proc setRepair(moduleIndex: int = -1) {.raises: [], tags: [], contractual.} =
  ## Remove or set the current player's ship's repair's priority
  ##
  ## * moduleIndex - the index of the module which will be repaired first. If
  ##                 -1, remove the priority
  playerShip.repairModule = moduleIndex
  if moduleIndex == -1:
    addMessage(message = "You removed the repair's priority.",
        mType = orderMessage)
  else:
    addMessage(message = "You assigned " & playerShip.modules[
        playerShip.repairModule].name & " as the repair's priority.",
        mType = orderMessage)

proc showGeneralInfo(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show the general info about the player's ship
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state - the current game's state
  ##
  ## Returns the modified parameters dialog and state.
  setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = "The name of your ship")
  label(str = "Name:")
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = "The name of your ship")
  colorLabel(str = playerShip.name, color = theme.colors[goldenColor])
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = "Set a new name for the ship")
  imageButton(image = images[editIcon]):
    dialog = renameDialog
  if playerShip.upgradeModule > -1:
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
    label(str = "Upgrade:")
    var
      upgradeInfo: string = playerShip.modules[
          playerShip.upgradeModule].name & " "
      maxUpgrade: int = 0
    case playerShip.modules[playerShip.upgradeModule].upgradeAction
    of durability:
      upgradeInfo.add(y = "(durability)")
      maxUpgrade = try:
          modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].durability
        except:
          dialog = setError(message = "Can't set max upgrade info.")
          return
    of maxValue:
      try:
        case modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].mType
        of engine:
          upgradeInfo.add(y = "(power)")
          maxUpgrade = (modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue / 20).int
        of cabin:
          upgradeInfo.add(y = "(quality)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue
        of gun, batteringRam:
          upgradeInfo.add(y = "(damage)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 2
        of hull:
          upgradeInfo.add(y = "(enlarge)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 40
        of harpoonGun:
          upgradeInfo.add(y = "(strength)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 10
        else:
          discard
      except:
        dialog = setError(message = "Can't set upgrade info.")
        return
    of value:
      try:
        if modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].mType == engine:
          upgradeInfo.add(y = "(fuel usage)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].value * 20
      except:
        dialog = setError(message = "Can't set upgrade fuel usage info.")
        return
    else:
      discard
    maxUpgrade = (maxUpgrade.float * newGameSettings.upgradeCostBonus).int
    if maxUpgrade == 0:
      maxUpgrade = 1
    var upgradePercent: int = 100 - ((playerShip.modules[
        playerShip.upgradeModule].upgradeProgress.float / maxUpgrade.float) * 100.0).int
    colorLabel(str = upgradeInfo, color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.9.cfloat, 0.1])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "The current ship's upgrade progress")
    progressBar(value = upgradePercent, maxValue = 100, modifyable = false)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Stop the current upgrade")
    imageButton(image = images[cancelIcon]):
      cancelUpgrade(dialog = dialog)
  if playerShip.repairModule > -1:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "If damaged, the module will be repaired as the first")
    label(str = "Repair first:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "If damaged, the module will be repaired as the first")
    colorLabel(str = playerShip.modules[playerShip.repairModule].name,
        color = theme.colors[goldenColor])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Remove the repair priority")
    imageButton(image = images[cancelIcon]):
      setRepair()
  if playerShip.destinationX > 0 and playerShip.destinationY > 0:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "The current travel destination of your ship")
    label(str = "Destination:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "The current travel destination of your ship")
    if skyMap[playerShip.destinationX][playerShip.destinationY].baseIndex > 0:
      colorLabel(str = skyBases[skyMap[playerShip.destinationX][
          playerShip.destinationY].baseIndex].name, color = theme.colors[goldenColor])
    else:
      colorLabel(str = "X: " & $playerShip.destinationX & " Y: " &
          $playerShip.destinationY, color = theme.colors[goldenColor])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Reset the ship destination")
    imageButton(image = images[cancelIcon]):
      playerShip.destinationX = 0
      playerShip.destinationY = 0
  setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Your ship the current home base")
  label(str = "Home:")
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Your ship the current home base")
  colorLabel(str = skyBases[playerShip.homeBase].name, color = theme.colors[goldenColor])
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = "Show the home base on map")
  imageButton(image = images[showIcon]):
    centerX = skyBases[playerShip.homeBase].skyX
    centerY = skyBases[playerShip.homeBase].skyY
    state = map
  setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "The ship weight. The more heavy is ship, the slower it fly and need stronger engines")
  label(str = "Weight:")
  try:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "The ship weight. The more heavy is ship, the slower it fly and need stronger engines")
    colorLabel(str = $countShipWeight(ship = playerShip) & "kg",
        color = theme.colors[goldenColor])
  except:
    dialog = setError(message = "Can't show the ship's weight")
    return
  setLayoutRowDynamic(height = 35, cols = 1)
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Your reputation among factions")
  label(str = "Reputation:")
  setLayoutRowDynamic(height = 35, cols = 2)
  for index, faction in factionsList:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show information about the faction")
    labelButton(title = faction.name):
      try:
        dialog = setInfo(text = faction.description[
            0..faction.description.rfind(sub = '\n') - 1], title = faction.name)
      except:
        dialog = setError(message = "Can't show information about the faction.")
        return
    let repLevel: int = getReputation(factionIndex = index)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Your reputation with the faction")
    colorLabel(str = getReputationText(reputationLevel = repLevel), color = (
        if repLevel > 0: theme.colors[greenColor] elif repLevel <
        0: theme.colors[redColor] else: theme.colors[goldenColor]))

proc showShipInfo*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with information about the player's ship
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  discard showManipulateItem(dialog = dialog)
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  if expandedSection == 0:
    setLayoutRowDynamic(height = height / 2, cols = 2)
  else:
    setLayoutRowDynamic(height = height, cols = 1)
  # General info about the player's ship
  if expandedSection in {0, 1}:
    group(title = "General info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship general info")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      showGeneralInfo(dialog = dialog, state = state)
  # The player's ship's crew info
  if expandedSection in {0, 2}:
    group(title = "Crew info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 2, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship crew info")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 2:
          expandedSection = 0
        else:
          expandedSection = 2
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show/Hide additional options related to managing the crew")
      imageButton(image = images[moreOptionsIcon]):
        showCrewOptions = not showCrewOptions
      showCrewInfo(dialog = dialog)
  # The player's ship's modules info
  if expandedSection in {0, 3}:
    group(title = "Modules info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship modules info")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 3:
          expandedSection = 0
        else:
          expandedSection = 3
      showModulesInfo(dialog = dialog)
  # The player's ship's cargo info
  if expandedSection in {0, 4}:
    group(title = "Cargo info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 2, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship cargo info")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 4:
          expandedSection = 0
        else:
          expandedSection = 4
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show/Hide additional options related to managing the cargo")
      imageButton(image = images[moreOptionsIcon]):
        showCargoOptions = not showCargoOptions
      showCargoInfo(dialog = dialog)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      height - 75)
