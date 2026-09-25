# Copyright 2025-2026 Bartek thindil Jasicki
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
import coreui, errordialog, header, knowledgebases, knowledgeevents,
    knowledgemissions, knowledgestories, messagesui, setui

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
  if showHeader(dialog = dialog, close = previous, state = state,
      options = knowledgeTab == 0):
    return
  # Show tab buttons
  changeStyle(field = spacing, x = 0, y = 0):
    changeStyle(field = buttonRounding, value = 0):
      setLayoutRowDynamic(height = tabHeight, cols = 4)
      const tabs: array[4, string] = ["Known bases", "Known events",
          "Accepted missions", "Known stories"]
      for index, tab in tabs:
        try:
          if knowledgeTab == index:
            changeStyle(src = active, dest = normal):
              labelButton(title = tab):
                discard
          else:
            labelButton(title = tab):
              knowledgeTab = index.cint
        except:
          dialog = setError(message = "Can't set the tabs buttons.")
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float - tabHeight)
  setLayoutRowDynamic(height = height, cols = 1)
  group(title = "Knowledge", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    case knowledgeTab
    # The list of known bases
    of 0:
      showBasesInfo(dialog = dialog)
    # The list of known events
    of 1:
      showEventsInfo(dialog = dialog)
    # The list of accepted missions
    of 2:
      showMissionsInfo(dialog = dialog)
    of 3:
    # The list of known stories
      showStoriesInfo(dialog = dialog)
    else:
      discard
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      height - 115, state = state)
