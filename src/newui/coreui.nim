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

## Provides various types and variables for the game's UI, like the game's
## state, the main window width and height, etc

type GameState* = enum
  ## Used to determine the current game's state.
  mainMenu, quitGame

var
  windowWidth*: Positive = 600 ## The width of the game's main window
  windowHeight*: Positive = 400 ## the height of the game's main window
