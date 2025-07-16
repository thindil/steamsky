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

## Provides code related to installing or removing modules from the player's
## ship, like showing the lists of modules, buying or selling them, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, errordialog, header, messagesui, setui, themes

var
  currentTab: cint = 0

proc showShipyard*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of modules to install or remove from the
  ## player's ship
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state,
      options = true):
    return
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "ShipyardGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    changeStyle(field = spacing, x = 0, y = 0):
      changeStyle(field = buttonRounding, value = 0):
        setLayoutRowDynamic(height = 30, cols = 2)
        const tabs: array[2, string] = ["Install modules", "Remove modules"]
        for index, tab in tabs:
          try:
            if currentTab == index:
              changeStyle(src = active, dest = normal):
                labelButton(title = tab):
                  discard
            else:
              labelButton(title = tab):
                currentTab = index.cint
          except:
            dialog = setError(message = "Can't set the tabs buttons.")
    # Show information about money owned by the player
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    for index, text in moneyText:
      if index mod 2 == 0:
        label(str = text)
      else:
        colorLabel(str = text, color = theme.colors[goldenColor])
    # Show information about installed modules
    setLayoutRowStatic(height = 30, cols = 5, ratio = modulesWidth)
    label(str = modulesText[0])
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
