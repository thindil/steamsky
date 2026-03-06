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

import std/strutils
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, types]
import coreui, errordialog, header, setui, themes

var
  keyLabel: string = ""
  keyIndex: ExtendedNatural = -1

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
        index: Natural; dialog: var GameDialog) {.raises: [],
        tags: [], contractual.} =
      ## Add an entry with a keyboard accelerator info and option to modify it
      ##
      ## * label   - the text to show on the accelerator
      ## * tooltip - the text to show as tooltip for the entry
      ## * value   - the current value of the selected accelerator
      ## * index   - the index of the key to set
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
        dialog = setKeyDialog

    type
      KeyTexts = object
        label, tooltip: string

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
    # Movement keys
    of 1:
      const keysTexts: array[14, KeyTexts] = [KeyTexts(
          label: "Move ship up/left", tooltip: "move ship up and left."),
          KeyTexts(label: "Move ship up", tooltip: "move ship up."), KeyTexts(
          label: "Move ship up/right", tooltip: "move ship up and right."),
          KeyTexts(label: "Move ship left", tooltip: "move ship left."),
          KeyTexts(label: "Wait in place or move 1 field",
          tooltip: "wait 1 minute or move 1 field."), KeyTexts(
          label: "Move ship right", tooltip: "move ship right."), KeyTexts(
          label: "Move ship down/left", tooltip: "move ship down and left."),
          KeyTexts(label: "Move ship down", tooltip: "move ship down."),
          KeyTexts(label: "Move ship down/right",
          tooltip: "move ship down and right."), KeyTexts(
          label: "Move ship to destination",
          tooltip: "move ship its destination."), KeyTexts(
          label: "Set full stop for ship",
          tooltip: "set full stop for the ship."), KeyTexts(
          label: "Set quarter speed for ship",
          tooltip: "set quarter speed for the ship."), KeyTexts(
          label: "Set half speed for ship",
          tooltip: "set half speed for the ship."), KeyTexts(
          label: "Set full speed for ship",
          tooltip: "set full speed for the ship.")]
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.25, 0.05])
      for index, key in movementKeysOptions.mpairs:
        addAccelerator(label = keysTexts[index].label & ":",
            tooltip = "Key used to " & keysTexts[index].tooltip,
            value = key, index = index, dialog = dialog)
      setLayoutRowDynamic(height = 30, cols = 1)
      labelButton(title = "Reset movement keys to default"):
        mapAccelerators[5] = "KP_Home"
        mapAccelerators[6] = "KP_Up"
        mapAccelerators[7] = "KP_Prior"
        mapAccelerators[8] = "KP_Left"
        mapAccelerators[9] = "KP_Begin"
        mapAccelerators[10] = "KP_Right"
        mapAccelerators[11] = "KP_End"
        mapAccelerators[12] = "KP_Down"
        mapAccelerators[13] = "KP_Next"
        mapAccelerators[14] = "KP_Divide"
        mapAccelerators[34] = "Control-a"
        mapAccelerators[35] = "Control-b"
        mapAccelerators[36] = "Control-c"
        mapAccelerators[37] = "Control-d"
        setMovementKeys()
    # Menu keys
    of 2:
      const keysTexts: array[12, KeyTexts] = [KeyTexts(
          label: "Ship information", tooltip: "show ship info screen."),
          KeyTexts(label: "Ship orders", tooltip: "show ship orders menu."),
          KeyTexts(label: "Crafting orders", tooltip: "show crafting screen."),
          KeyTexts(label: "Last messages", tooltip: "show messages screen."),
          KeyTexts(label: "Knowledge list", tooltip: "show knowledge screen."),
          KeyTexts(label: "Wait orders", tooltip: "show wait orders menu."),
          KeyTexts(label: "Game statistics",
          tooltip: "show game statistics screen."), KeyTexts(label: "Help",
          tooltip: "show help screen."), KeyTexts(label: "Game options",
          tooltip: "show game options screen."), KeyTexts(
          label: "Quit from game", tooltip: "quit from the game"), KeyTexts(
          label: "Resign from game", tooltip: "resign from the game."),
          KeyTexts(label: "Show menu", tooltip: "show main menu.")]
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.25, 0.05])
      for index, key in menuKeysOptions.mpairs:
        addAccelerator(label = keysTexts[index].label & ":",
            tooltip = "Key used to " & keysTexts[index].tooltip,
            value = key, index = index, dialog = dialog)
      setLayoutRowDynamic(height = 30, cols = 1)
      labelButton(title = "Reset menu keys to default"):
        menuAccelerators[1] = "s"
        menuAccelerators[2] = "o"
        menuAccelerators[3] = "r"
        menuAccelerators[4] = "m"
        menuAccelerators[5] = "k"
        menuAccelerators[6] = "w"
        menuAccelerators[7] = "g"
        menuAccelerators[8] = "F1"
        menuAccelerators[9] = "p"
        menuAccelerators[10] = "q"
        menuAccelerators[11] = "x"
        mapAccelerators[1] = "e"
        setMenuKeys()
    # Map keys
    of 3:
      const keysTexts: array[22, KeyTexts] = [KeyTexts(
          label: "Center map on player ship",
          tooltip: "center map on player ship."), KeyTexts(
          label: "Center map on home base",
          tooltip: "center map on home base."), KeyTexts(
          label: "Move map to left", tooltip: "move map left."), KeyTexts(
          label: "Move map to right", tooltip: "move map right."), KeyTexts(
          label: "Move map up", tooltip: "move map up."), KeyTexts(
          label: "Move map down", tooltip: "move map down."), KeyTexts(
          label: "Move map up/left", tooltip: "move map up and left."),
          KeyTexts(label: "Move map up/right",
          tooltip: "move map up and right."), KeyTexts(
          label: "Move map down/left", tooltip: "move map down and left."),
          KeyTexts(label: "Move map down/right",
          tooltip: "move map down and left."), KeyTexts(
          label: "Move cursor up/left", tooltip: "move cursor up and left."),
          KeyTexts(label: "Move cursor up", tooltip: "move cursor up."),
          KeyTexts(label: "Move cursor up/right",
          tooltip: "move cursor up and right."), KeyTexts(
          label: "Move cursor left", tooltip: "move cursor left."), KeyTexts(
          label: "Move cursor right", tooltip: "move cursor right."), KeyTexts(
          label: "Move cursor down/left", tooltip: "move cursor down left."),
          KeyTexts(label: "Move cursor down", tooltip: "move cursor down."),
          KeyTexts(label: "Move cursor down/right",
          tooltip: "move cursor down and right."), KeyTexts(
          label: "Press mouse button", tooltip: "emulate mouse button."),
          KeyTexts(label: "Zoom in map", tooltip: "zoom in map."), KeyTexts(
          label: "Zoom out map", tooltip: "zoom out map."), KeyTexts(
          label: "Show move map options", tooltip: "show move map options.")]
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.25, 0.05])
      for index, key in mapKeysOptions.mpairs:
        addAccelerator(label = keysTexts[index].label & ":",
            tooltip = "Key used to " & keysTexts[index].tooltip,
            value = key, index = index, dialog = dialog)
      setLayoutRowDynamic(height = 30, cols = 1)
      labelButton(title = "Reset map keys to default"):
        mapAccelerators[15] = "Shift-Return"
        mapAccelerators[16] = "Shift-h"
        mapAccelerators[20] = "Shift-KP_Left"
        mapAccelerators[21] = "Shift_KP_Right"
        mapAccelerators[18] = "Shift-KP_Up"
        mapAccelerators[23] = "Shift-KP_Down"
        mapAccelerators[17] = "Shift-KP_Home"
        mapAccelerators[19] = "Shift-KP_Prior"
        mapAccelerators[22] = "Shift-KP_End"
        mapAccelerators[24] = "Shift-KP_Next"
        mapAccelerators[25] = "Control-KP_Home"
        mapAccelerators[26] = "Control-KP_Up"
        mapAccelerators[27] = "Control-KP_Prior"
        mapAccelerators[28] = "Control-KP_Left"
        mapAccelerators[29] = "Control-KP_Right"
        mapAccelerators[30] = "Control-KP_End"
        mapAccelerators[31] = "Control-KP_Down"
        mapAccelerators[32] = "Control-KP_Next"
        mapAccelerators[33] = "Control-Return"
        mapAccelerators[3] = "+"
        mapAccelerators[4] = "-"
        mapAccelerators[2] = "v"
        setMapKeys()
    # General keys
    of 4:
      const keysTexts: array[4, KeyTexts] = [KeyTexts(
          label: "Resize first section",
          tooltip: "resize (maximize or minimize) the first section of information (like ship info, knowledge or in combat)."),
          KeyTexts(label: "Resize second section",
          tooltip: "resize (maximize or minimize) the second section of information (like ship info, knowledge or in combat)."),
          KeyTexts(label: "Resize third section",
          tooltip: "resize (maximize or minimize) the third section of information (like ship info, knowledge or in combat)."),
          KeyTexts(label: "Resize fourth section",
          tooltip: "resize (maximize or minimize) the fourth section of information (like ship info, knowledge or in combat).")]
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.25, 0.05])
      for index, key in generalKeysOptions.mpairs:
        addAccelerator(label = keysTexts[index].label & ":",
            tooltip = "Key used to " & keysTexts[index].tooltip,
            value = key, index = index, dialog = dialog)
      setLayoutRowDynamic(height = 30, cols = 1)
      labelButton(title = "Reset general keys to default"):
        mapAccelerators[34] = "Control-a"
        mapAccelerators[35] = "Control-b"
        mapAccelerators[36] = "Control-c"
        mapAccelerators[37] = "Control-d"
        setGeneralKeys()
    # Interface options
    of 5:
      setLayoutRowDynamic(height = 30, cols = 2)
      addComboList(label = "Interace theme:", tooltip = "Select UI theme.",
          items = interfaceThemes, value = interfaceOptions[0])
      addCheckbox(label = "Use right mouse button:",
          option = interfaceOptions[1],
          tooltip = "Use right mouse button to show various menus in the game.")
      addCheckbox(label = "Show tooltips:",
          option = interfaceOptions[2],
          tooltip = "Show help tooltips for various game elements.")
      addCheckbox(label = "Show last messages:",
          option = interfaceOptions[3],
          tooltip = "Show last messages window in every place in the game.")
      addCheckbox(label = "Full screen mode:",
          option = interfaceOptions[4],
          tooltip = "Run the game in full screen mode.")
      setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.4.cfloat, 0.25, 0.05])
      addAccelerator(label = "Full screen shortcut:",
          tooltip = "Key used to switch full screen mode.",
          value = fullScreenAccel, index = 0, dialog = dialog)
      setLayoutRowDynamic(height = 30, cols = 2)
      addProperty(label = "Close messages after:",
          tooltip = "Auto close game messages after that amount of seconds.",
          min = 1, max = 60, value = interfaceOptions[5])
      addCheckbox(label = "Show numeric values:",
          option = interfaceOptions[6],
          tooltip = "Show numeric values of many statistics, like crew abilities, weapons strength, etc.")
      addProperty(label = "Amount items on lists:",
          tooltip = "The amount of items displayed on various lists in the game like crew members, modules, etc.",
          min = 5, max = 100, value = interfaceOptions[7])
      addProperty(label = "Size of map font:",
          tooltip = "Size (in pixels) of font used to draw game map.",
          min = 3, max = 50, value = interfaceOptions[8])
    else:
      discard
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
      var keyPressed: Keys = keyNone
      for key in keyScrollDown..keyBackspace:
        if isKeyPressed(key = key):
          keyPressed = key
          break
      if keyPressed != keyNone or getInputTextLen() > 0:
        var key: SettingString = ""
        if getInputTextLen() > 0:
          key = getInputText()
          if isUpperAscii(c = key[0]):
            key = "Shift-" & key.toLowerAscii()
        elif keyPressed notin {keyNone, keyEscape, keyTab}:
          key = $keyPressed
        if isKeyPressed(key = keyCtrl):
          key = "Control-" & key
        if isKeyPressed(key = keyAlt):
          key = "Alt-" & key
        if key.len > 0:
          case currentTab
          of 1:
            movementKeysOptions[keyIndex] = key
          of 2:
            menuKeysOptions[keyIndex] = key
          of 3:
            mapKeysOptions[keyIndex] = key
          of 4:
            generalKeysOptions[keyIndex] = key
          of 5:
            fullScreenAccel = key
          else:
            discard
        keyIndex = -1
        keyLabel = ""
        dialog = none
