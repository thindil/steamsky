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
    setLayoutRowDynamic(height = 30, cols = 2)
    case currentTab
    # General options
    of 0:
      label(str = "Auto rest when crew is tired:")
      var checked: bool = generalOptions[0].bool
      checkbox(label = "", checked = checked)
      generalOptions[0] = checked.ord
      label(str = "Default speed after undocking:")
      var selected: Natural = generalOptions[1] - 1
      generalOptions[1] = (comboList(items = shipSpeeds, selected = selected,
          itemHeight = 25, x = 350, y = 200) + 1)
      label(str = "Auto center map after set destination:")
      checked = generalOptions[2].bool
      checkbox(label = "", checked = checked)
      generalOptions[2] = checked.ord
      label(str = "Auto set base after finished mission:")
      checked = generalOptions[3].bool
      checkbox(label = "", checked = checked)
      generalOptions[3] = checked.ord
      label(str = "Auto set destination after accepting mission:")
      checked = generalOptions[4].bool
      checkbox(label = "", checked = checked)
      generalOptions[4] = checked.ord
      label(str = "Auto finish mission:")
      checked = generalOptions[5].bool
      checkbox(label = "", checked = checked)
      generalOptions[5] = checked.ord
      label(str = "Auto ask for bases:")
      checked = generalOptions[6].bool
      checkbox(label = "", checked = checked)
      generalOptions[6] = checked.ord
      label(str = "Auto ask for events:")
      checked = generalOptions[7].bool
      checkbox(label = "", checked = checked)
      generalOptions[7] = checked.ord
    else:
      discard
