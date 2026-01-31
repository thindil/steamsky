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

## Provides code related to the game's help, like showing it, etc.

import std/tables
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, help]
import coreui, header, setui, themes

proc showHelp*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with in-game help
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  setLayoutRowDynamic(height = gameSettings.topicsPosition.float, cols = 1)
  group(title = "TopicsGroup", flags = {windowNoFlags}):
    setLayoutRowDynamic(height = 25, cols = 1)
    var index: Natural = 0
    for title, entry in helpList:
      var sel: bool = selectedHelp == index
      if selectableLabel(str = title, value = sel):
        if sel:
          selectedHelp = index
          setHelpContent(content = entry.text, dialog = dialog)
      index.inc
  setLayoutRowDynamic(height = 20, cols = 2)
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the list of topics smaller.")
  imageButtonCentered(image = images[contract2Icon]):
    gameSettings.topicsPosition -= gameSettings.interfaceFontSize + 10
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the list of topics bigger.")
  imageButtonCentered(image = images[expand2Icon]):
    gameSettings.topicsPosition += gameSettings.interfaceFontSize + 10
  setLayoutRowDynamic(height = windowHeight -
      gameSettings.topicsPosition.float - 75, cols = 1)
  group(title = "ContentGroup", flags = {windowNoFlags}):
    for row in helpContent:
      var ratio: seq[cfloat] = @[]
      for lbl in row:
        ratio.add(y = lbl.width)
      setLayoutRowStatic(height = 25, cols = row.len, ratio = ratio)
      for lbl in row:
        case lbl.tag
        of none:
          label(str = lbl.text)
        of underline:
          colorLabel(str = lbl.text, color = theme.helpColors[underlineHelpColor])
        of bold:
          colorLabel(str = lbl.text, color = theme.helpColors[boldHelpColor])
        of italic:
          colorLabel(str = lbl.text, color = theme.helpColors[italicHelpColor])
        else:
          label(str = lbl.text)
