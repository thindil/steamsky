# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides some constants and variables widely used in the game's UI.

const
  mainPaned*: string = ".gameframe.paned"
    ## The name of the main Tk paned widget of the game
  gameHeader*: string = ".gameframe.header"
    ## The name of the header Tk frame (with menu button, time, icons, etc.)
  closeButton*: string = gameHeader & ".closebutton"
    ## The name of the button used to close all screens in the game

var
  generalAccelerators*: array[4, string] = ["Alt-a", "Alt-b", "Alt-c", "Alt-d"]
    ## The list of keyboard shortcuts used in some places
  startX*, startY*: int = 0
    ## Coordinates of the top left point on the map
  centerX*, centerY*: Positive = 1
    ## Coordinates of the center point on the map
