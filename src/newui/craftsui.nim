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

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, errordialog, header, setui

var
  hasOptions: bool = true

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
    let newWorkshop: Natural = comboList(items = workshopsList, selected = workshopType,
        itemHeight = 25, x = 200, y = 150)
    if newWorkshop != workshopType:
      workshopType = newWorkshop
