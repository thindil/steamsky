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

## Provides code related to the information about the player's character's
## knowledge, like minimizing/maximizing its sections, drawing the UI, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../[config]
import coreui, header, knowledgebases, knowledgeevents, knowledgemissions, knowledgestories, messagesui, themes

var expandedSection: Natural = 0

proc showKnowledge*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with information about the player's character's knowledge
  ## about the world
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if mapPreview:
    state = map
    return
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  if expandedSection == 0:
    setLayoutRowDynamic(height = height / 2, cols = 2)
  else:
    setLayoutRowDynamic(height = height, cols = 1)
  # The list of known bases
  if expandedSection in {0, 1}:
    group(title = "Known bases:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 2, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the list of known bases")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show/Hide additional options related to managing the list of known bases")
      imageButton(image = images[moreOptionsIcon]):
        showBasesOptions = not showBasesOptions
      showBasesInfo(dialog = dialog)
  # The list of known events
  if expandedSection in {0, 2}:
    group(title = "Known events:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 2, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the list of known events")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 2:
          expandedSection = 0
        else:
          expandedSection = 2
      showEventsInfo(dialog = dialog)
  # The list of accepted missions
  if expandedSection in {0, 3}:
    group(title = "Accepted missions:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the list of accepted missions")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 3:
          expandedSection = 0
        else:
          expandedSection = 3
      showMissionsInfo(dialog = dialog)
  # The list of known stories
  if expandedSection in {0, 4}:
    group(title = "Known stories:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 2, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the list of known stories")
      imageButton(image = (if expandedSection == 0: images[
          expandIcon] else: images[contractIcon])):
        if expandedSection == 4:
          expandedSection = 0
        else:
          expandedSection = 4
      showStoriesInfo(dialog = dialog)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      height - 75)
