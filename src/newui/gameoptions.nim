# Copyright 2026 Bartek thindil Jasicki
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

## Provides code related to the game's options, like showing it, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, errordialog, header, setui

proc showOptions*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with game options
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  # Show tab buttons
  changeStyle(field = spacing, x = 0, y = 0):
    changeStyle(field = buttonRounding, value = 0):
      const tabs: array[7, string] = ["General", "Movement keys", "Menu keys",
          "Map keys", "General keys", "Interface", "Info"]
      setLayoutRowDynamic(height = 30, cols = tabs.len)
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
  setLayoutRowDynamic(height = windowHeight - 75, cols = 1)
  group(title = "OptionsGroup", flags = {windowNoFlags}):

    proc addCheckbox(label: string; option: var Natural;
        tooltip: string) {.raises: [], tags: [], contractual.} =
      ## Add a checkbox to the list of options
      ##
      ## * label   - the text to show on the checkbox
      ## * option  - the value for the selected checkbox
      ## * tooltip - the text to show as tooltip for the checkbox
      ##
      ## Returns the modified parameter option
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      label(str = label)
      var checked: bool = option.bool
      checkbox(label = "", checked = checked)
      option = checked.ord

    setLayoutRowDynamic(height = 30, cols = 2)
    case currentTab
    # General options
    of 0:
      addCheckbox(label = "Auto rest when crew is tired:",
          option = generalOptions[0],
          tooltip = "Wait for crew is rested when pilot or engineer are too tired to work.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Default speed of ship after undock from base.")
      label(str = "Default speed after undocking:")
      var selected: Natural = generalOptions[1] - 1
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Default speed of ship after undock from base.")
      generalOptions[1] = (comboList(items = shipSpeeds, selected = selected,
          itemHeight = 25, x = 350, y = 200) + 1)
      addCheckbox(label = "Auto center map after set destination:",
          option = generalOptions[2],
          tooltip = "After set destination for ship, center map on ship.")
      addCheckbox(label = "Auto set base after finished mission:",
          option = generalOptions[3],
          tooltip = "After finished mission, set skybase from which mission was taken as a destination for ship.")
      addCheckbox(label = "Auto set destination after accepting mission:",
          option = generalOptions[4], tooltip = "")
      addCheckbox(label = "Auto finish mission:", option = generalOptions[5], tooltip = "")
      addCheckbox(label = "Auto ask for bases:", option = generalOptions[6], tooltip = "")
      addCheckbox(label = "Auto ask for events:", option = generalOptions[7], tooltip = "")
    else:
      discard
