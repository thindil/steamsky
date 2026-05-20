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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the wait some time menu, like showing the menu,
## executing a wait command, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crew2, game, game2, shipsmovement, types]
import coreui, errordialog

type
  WaitReason = enum
    rest, heal

var
  waitAmount: Positive = 1
  waitInterval: Natural = 0
  needRest, needHealing: bool = false

proc setWaitMenu*() {.raises: [], tags: [], contractual.} =
  ## Set the buttons to wait until crew is rested or healed
  needRest = false
  needHealing = false
  for index, member in playerShip.crew:
    if member.tired > 0 and member.order == rest:
      needRest = true
    if member.health in 1 .. 99 and member.order == rest:
      for module in playerShip.modules:
        if module.mType == ModuleType2.cabin:
          for owner in module.owner:
            if owner == index:
              needHealing = true
              break

proc wait(minutes: Positive): GameDialog {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Wait in place by selected in-game minutes
  ##
  ## * minutes - the amount of minutes to wait
  try:
    updateGame(minutes = minutes)
    waitInPlace(minutes = minutes)
    setWaitMenu()
  except:
    return setError(message = "Can't wait in place.")
  return none

proc waitReason(reason: WaitReason): GameDialog {.raises: [], tags: [
    WriteIOEffect, RootEffect], contractual.} =
  ## Wait in place for some time, depends on the reason
  ##
  ## * reason - the reason to wait, resting or wounded crew members
  if reason == rest:
    try:
      waitForRest()
      setWaitMenu()
      return none
    except:
      return setError(message = "Can't wait until crew is rested.")
  else:
    var timeNeeded: Natural = 0
    for index, member in playerShip.crew:
      if member.health in 1..99 and member.order == rest:
        block checkModules:
          for module in playerShip.modules:
            if module.mType == ModuleType2.cabin:
              for owner in module.owner:
                if owner == index:
                  if timeNeeded < (100 - member.health) * 15:
                    timeNeeded = (100 - member.health) * 15
                    break checkModules
    if timeNeeded == 0:
      return none
    return wait(minutes = timeNeeded)

proc showWaitMenu*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the menu with options to wait some in-game time
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened or menu has closed.

  const windowName: string = "Wait in place"
  var height: float = 320
  if needRest:
    height += 34
  if needHealing:
    height += 34
  window(name = windowName, x = windowWidth / 4, y = windowHeight / 4,
      w = 320, h = height, flags = {windowBorder, windowTitle,
      windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for 1 minute")
    labelButton(title = "Wait 1 minute"):
      dialog = wait(minutes = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for 5 minutes")
    labelButton(title = "Wait 5 minutes"):
      dialog = wait(minutes = 5)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for 10 minutes")
    labelButton(title = "Wait 10 minutes"):
      dialog = wait(minutes = 10)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for 15 minutes")
    labelButton(title = "Wait 15 minutes"):
      dialog = wait(minutes = 15)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for 30 minutes")
    labelButton(title = "Wait 30 minutes"):
      dialog = wait(minutes = 30)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for 1 hour")
    labelButton(title = "Wait 1 hour"):
      dialog = wait(minutes = 60)
    setLayoutRowDynamic(height = 30, cols = 3)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for the selected amount of minutes: from 1 to 1440 (the whole day)")
    labelButton(title = "Wait"):
      case waitInterval
      of 0:
        dialog = wait(minutes = waitAmount)
      of 1:
        dialog = wait(minutes = waitAmount * 60)
      of 2:
        dialog = wait(minutes = waitAmount * 1440)
      else:
        discard
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Wait in place for the selected amount of time: from 1 to 1440")
    let newValue: int = property2(name = "#", min = 1, val = waitAmount,
        max = 1440, step = 1, incPerPixel = 1)
    if newValue != waitAmount:
      waitAmount = newValue
    waitInterval = comboList(items = ["minutes", "hours", "days"],
        selected = waitInterval, itemHeight = 25, x = 100, y = 180)
    setLayoutRowDynamic(height = 30, cols = 1)
    if needRest:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Wait in place until the whole ship's crew is rested")
      labelButton(title = "Wait until crew is rested"):
        dialog = waitReason(reason = rest)
    if needHealing:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Wait in place until the whole ship's crew is healed. Can take a large amount of time")
      labelButton(title = "Wait until crew is healed"):
        dialog = waitReason(reason = heal)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Close dialog")
    labelButton(title = "Close"):
      dialog = none

  windowSetFocus(name = windowName)
