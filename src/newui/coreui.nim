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

import nuklear/nuklear_sdl_renderer

type GameState* = enum
    ## Used to determine the current game's state.
    quitGame, mainMenu, news, allNews, about, showFile, hallOfFame, loadGame,
        loadingGame, map, newGame

type GameDialog* = enum
    ## Used to show any in-game dialog window
    none, errorDialog, loadMenu, questionDialog, goalsDialog

const
    dtime*: float = 40.0 ## The length in miliseconds of one game's frame

var
    fonts*: seq[ptr nk_font] = @[] ## The list of fonts used by the game
