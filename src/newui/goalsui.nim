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

## Provides code related to the selecting the player's goal dialog, like
## showing the dialog, and selecting it.

import std/tables
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game]
import coreui

var selectedGoal*: string = "Random"

proc showGoals*(dialog: var GameDialog) {.raises: [], tags: [], contractual.} =
  ## Show the dialog with the list of available goals for players
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  window(name = "Select a new goal", x = 40, y = 20, w = 540, h = 360, flags = {
      windowBorder, windowMoveable, windowTitle, windowMinimizable,
      windowNoScrollbar}):
    setLayoutRowDynamic(height = 230, cols = 1)
    group(title = "GoalsGroup", flags = {windowNoFlags}):
      setLayoutRowDynamic(height = 25, cols = 1)
      for goal in goalsList.values:
        discard
    setLayoutRowDynamic(height = 35, cols = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Select the goal for your character from the list. If you choose Random option, a random goal will be assigned. You can always change it later during the game, but you will lose all progress then.")
    labelButton(title = "Select goal"):
      dialog = none
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Close the goals list without any changes")
    labelButton(title = "Close"):
      dialog = none
