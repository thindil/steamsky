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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's main menu, like showing the
## menu, and selecting its various sections

import contracts, nuklear/nuklear_sdl_renderer
import coreui

proc showMainMenu*(state: var GameState) {.raises: [], tags: [], contractual.} =
  ## Show the game's main menu and set the game's state
  ##
  ## * state - the current game's state
  ##
  ## Returns the modified parameter state.
  window(name = "MainMenu", x = 240, y = 100, w = 140, h = (
      windowHeight - 20).float, flags = {windowNoFlags}):
    setLayoutRowDynamic(height = 40, cols = 1)
    labelButton(title = "New game"):
      echo "button pressed"
    labelButton(title = "Load game"):
      echo "button pressed"
    labelButton(title = "Hall of Fame"):
      echo "button pressed"
    labelButton(title = "News"):
      echo "button pressed"
    labelButton(title = "About"):
      echo "button pressed"
    labelButton(title = "Quit"):
      state = quitGame
      return
  state = mainMenu
