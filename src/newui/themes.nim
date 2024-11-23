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

## Provides code related to the game's themes, like loading them

import nuklear/nuklear_sdl_renderer
import ../config

type
  Theme = object
    ## Used to store data about the game's themes
    toolButton: ButtonStyle ## Style for toolbuttons type of buttons

var
  defaultTheme: Theme = Theme() ## The default game's theme
  currentTheme*: Theme = Theme() ## The current theme used by the game

proc setThemes*() =
  ## Set the default and current themes of the game
  if gameSettings.interfaceTheme == "steamsky":
    currentTheme = defaultTheme
