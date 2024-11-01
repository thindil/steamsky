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

## Provides code related to the game's UI, like tooltips

import contracts, nuklear/nuklear_sdl_renderer
import coreui

const
  tooltipDelay: float = 1000.0
    ## For how long the player hovers the mouse over UI element before a
    ## tooltip will be shown

var
  tooltipTime: float = tooltipDelay ## How long left to show a tooltip

proc showTooltip*(bounds: NimRect; text: string) {.raises: [], tags: [],
    contractual.} =
  if isMouseHovering(bounds):
    tooltipTime -= dtime
    if tooltipTime <= 0.0:
      tooltip("Set and start a new game")
  else:
    tooltipTime = tooltipDelay
