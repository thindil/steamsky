# Copyright 2024 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's main map, like, creating the game's UI,
## etc.

import contracts, nuklear/nuklear_sdl_renderer
import coreui

proc createGameUi*() {.raises: [], tags: [], contractual.} =
  ## Create the game's UI and show the map to the player
  discard

proc showMap*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [], contractual.} =
  ## Show the game's map
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  layoutDynamic(30, 3):
    row(0.1):
      labelButton(title = "menu"):
        discard
    row(0.4):
      label(str = "Time")
    row(0.5):
      setLayoutRowDynamic(30, 2)
      label(str = "test")
      label(str = "test2")
  dialog = none
  state = map
