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

import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game]
import coreui, header, messagesui, themes

var expandedSection: Natural = 0

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
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      setLayoutRowDynamic(height = 35, cols = 3)
      label(str = "Name:")
      colorLabel(str = playerShip.name, color = theme.colors[goldenColor])
      labelButton(title = "s"):
          discard
  # The player's ship's crew info
  if expandedSection in {0, 2}:
    group(title = "Crew info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship crew info")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 2:
          expandedSection = 0
        else:
          expandedSection = 2
  # The player's ship's modules info
  if expandedSection in {0, 3}:
    group(title = "Modules info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship modules info")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 3:
          expandedSection = 0
        else:
          expandedSection = 3
  # The player's ship's cargo info
  if expandedSection in {0, 4}:
    group(title = "Cargo info:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship cargo info")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 4:
          expandedSection = 0
        else:
          expandedSection = 4
  showLastMessages(theme = theme, dialog = dialog, inCombat = true,
    height = windowHeight - height - 75)
