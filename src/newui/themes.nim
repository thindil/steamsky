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

## Provides code related to the game's themes' system, like default
## theme setting, etc

import std/os
import contracts, nuklear/nuklear_sdl_renderer
import ../game

type
  ThemeData* = object
    ## Stores data about the game's theme
    name: string
    fileName: string
    menuIcons*: array[4, string]
    mapIcons*: array[4, string]

let defaultTheme: ThemeData = ThemeData(name: "default",
    fileName: dataDirectory & "ui" & DirSep & "theme.txt", menuIcons: ["", "",
    "", ""], mapIcons: ["", "", "", ""])

proc loadTheme*() {.raises: [], tags: [], contractual.} =
  ## Set the theme for the game
  discard
