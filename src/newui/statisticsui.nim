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

## Provides code related to the game's statistics ui, like showing them, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, header, messagesui

proc showStatistics*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with information about the current game's statistics
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  setLayoutRowDynamic(height = height, cols = 2)
  group(title = "Group1", flags = {}):
    discard
  group(title = "Group2", flags = {}):
    discard
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      height - 75)
