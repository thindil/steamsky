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

    proc addProperty(label, tooltip: string; min, max: Natural;
        value: var Natural) {.raises: [], tags: [], contractual.} =
      ## Add a property to the list of options
      ##
      ## * label   - the text to show on the property
      ## * tooltip - the text to show as tooltip for the property
      ## * min     - the minimal value to set on the property
      ## * max     - the maximum value to set on the property
      ## * value   - the current value set on the property
      ##
      ## Returns the modified parameter value
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      label(str = label)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      value = property2(name = "#", min = min, val = value, max = max, step = 1,
          incPerPixel = 1)


    proc addComboList(label, tooltip: string; items: openArray[string];
        value: var Natural) {.raises: [], tags: [], contractual.} =
      ## Add a combo list to the list of options
      ##
      ## * label   - the text to show on the combo list
      ## * tooltip - the text to show as tooltip for the combo list
      ## * item    - the list of items to show in the combo list
      ## * value   - the currently selected value on the list
      ##
      ## Returns the modified parameter value
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      label(str = label)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      value = comboList(items = shipSpeeds, selected = value, itemHeight = 25,
          x = 350, y = 200)

    setLayoutRowDynamic(height = 30, cols = 2)
    case currentTab
    # General options
    of 0:
      addCheckbox(label = "Auto rest when crew is tired:",
          option = generalOptions[0],
          tooltip = "Wait for crew is rested when pilot or engineer are too tired to work.")
      var selected: Natural = generalOptions[1] - 1
      addComboList(label = "Default speed after undocking:",
          tooltip = "Default speed of ship after undock from base.",
          items = shipSpeeds, value = selected)
      generalOptions[1] = selected + 1
      addCheckbox(label = "Auto center map after set destination:",
          option = generalOptions[2],
          tooltip = "After set destination for ship, center map on ship.")
      addCheckbox(label = "Auto set base after finished mission:",
          option = generalOptions[3],
          tooltip = "After finished mission, set skybase from which mission was taken as a destination for ship.")
      addCheckbox(label = "Auto set destination after accepting mission:",
          option = generalOptions[4], tooltip = "After accepting a mission, set its target as a destination for ship.")
      addCheckbox(label = "Auto finish mission:", option = generalOptions[5],
          tooltip = "Auto finish missions when ship is near corresponding skybase. Missions will not be finished if there is no trader on position or when there is Double Price event in the base.")
      addCheckbox(label = "Auto ask for bases:", option = generalOptions[6],
          tooltip = "Auto ask for bases when ship end docking to bases.")
      addCheckbox(label = "Auto ask for events:", option = generalOptions[7],
          tooltip = "Auto ask for events when ship end docking to bases.")
      addProperty(label = "Low level of fuel:",
          tooltip = "Amount of fuel below which you will see warning about low level of. Enter value between 1 and 10 000.",
          min = 1, max = 10_000, value = generalOptions[8])
      addProperty(label = "Low level of drinks:",
          tooltip = "Amount of drinks below which you will see warning about low level of. Enter value between 1 and 10 000.",
          min = 1, max = 10_000, value = generalOptions[9])
      addProperty(label = "Low level of food:",
          tooltip = "Amount of food below which you will see warning about low level of. Enter value between 1 and 10 000.",
          min = 1, max = 10_000, value = generalOptions[10])
    else:
      discard
