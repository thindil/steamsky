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
import ../[config, types]
import coreui, errordialog, header, setui, themes

type
  KeysType = enum
    none, movementKeys, menuKeys, mapKeys, generalKeys

var
  keyLabel: string = ""
  keyIndex: ExtendedNatural = -1
  keyType: KeysType = none

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
      value = comboList(items = items, selected = value, itemHeight = 25,
          x = 350, y = 200)

    proc addAccelerator(label, tooltip: string; value: var string;
        index: Natural; kType: KeysType; dialog: var GameDialog) {.raises: [],
        tags: [], contractual.} =
      ## Add an entry with a keyboard accelerator info and option to modify it
      ##
      ## * label   - the text to show on the accelerator
      ## * tooltip - the text to show as tooltip for the entry
      ## * value   - the current value of the selected accelerator
      ## * index   - the index of the key to set
      ## * kType   - the type of key to set
      ##
      ## Returns the modified parameter value
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      label(str = label)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = tooltip)
      label(str = value)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set the new keyboard shortcut for the action")
      imageButton(image = images[moreOptionsIcon]):
        keyLabel = label
        keyIndex = index
        keyType = kType
        dialog = setKeyDialog

    case currentTab
    # General options
    of 0:
      setLayoutRowDynamic(height = 30, cols = 2)
      const
        autoMoveList: array[4, string] = ["Never", "Any ship",
          "Friendly ship", "Enemy ship"]
        messagesOrderList: array[2, string] = ["Older messages first", "Newer messages first"]
        autoSaveList: array[6, string] = ["Never", "After dock to base",
            "After undock from base", "Every game day", "Every game month", "Every game year"]

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
      addComboList(label = "Stop auto movement:",
          tooltip = "Set when auto move ship should stop: never, on meet any ship, on meet friendly ship or on meet enemy ship.",
          items = autoMoveList, value = generalOptions[11])
      addComboList(label = "Messages order:",
          tooltip = "In what order show messages in game. If Older first will be select, then older messages will appear at top\nof the lists. Otherwise newer messages will be at top.",
          items = messagesOrderList, value = generalOptions[14])
      addComboList(label = "Auto save:",
          tooltip = "How often game should be automatically saved to disk.",
          items = autoSaveList, value = generalOptions[15])
      addProperty(label = "Low level of fuel:",
          tooltip = "Amount of fuel below which you will see warning about low level of. Enter value between 1 and 10 000.",
          min = 1, max = 10_000, value = generalOptions[8])
      addProperty(label = "Low level of drinks:",
          tooltip = "Amount of drinks below which you will see warning about low level of. Enter value between 1 and 10 000.",
          min = 1, max = 10_000, value = generalOptions[9])
      addProperty(label = "Low level of food:",
          tooltip = "Amount of food below which you will see warning about low level of. Enter value between 1 and 10 000.",
          min = 1, max = 10_000, value = generalOptions[10])
      addProperty(label = "Messages limit:",
          tooltip = "Amount of messages stored in game. If new message arrive when limit is reached, oldest message will be deleted. Enter value between 10 and 5000.",
          min = 1, max = 5_000, value = generalOptions[12])
      addProperty(label = "Saved messages:",
          tooltip = "Maximum amount of last messages saved to file. Enter value between 5 and 200.",
          min = 1, max = 200, value = generalOptions[13])
      addProperty(label = "Wait time:",
          tooltip = "How much minutes will pass after press the Wait button.. Enter value between 1 and 1440.",
          min = 1, max = 1_440, value = generalOptions[16])
    else:
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.15, 0.05])
      addAccelerator(label = "Move ship up/left:",
          tooltip = "Key used to move ship up and left.",
          value = movementKeysOptions[0], index = 0, kType = movementKeys,
          dialog = dialog)
      # Start setting the selected key
      if dialog == setKeyDialog:
        try:
          popup(pType = staticPopup, title = "Set Key", flags = {windowNoFlags},
              x = windowWidth / 4, y = windowHeight / 4, w = windowWidth / 2, h = 120):
            setLayoutRowDynamic(height = 100, cols = 1)
            wrapLabel(str = "Press a key or keys combination to set it as a new value for " &
                keyLabel & ". Press Escape to cancel.")
        except NuklearException:
          dialog = setError(message = "Can't create a popup")
        if isKeyPressed(key = keyEscape) or getInputTextLen() > 0:
          if getInputTextLen() > 0:
            case keyType
            of movementKeys:
              movementKeysOptions[keyIndex] = getInputText()
              if isKeyPressed(key = keyCtrl):
                movementKeysOptions[keyIndex] = "Ctrl-" & movementKeysOptions[keyIndex]
              if isKeyPressed(key = keyAlt):
                movementKeysOptions[keyIndex] = "Alt-" & movementKeysOptions[keyIndex]
            else:
              discard
          keyIndex = -1
          keyLabel = ""
          keyType = none
          dialog = none
