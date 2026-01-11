# Copyright 2024-2025 Bartek thindil Jasicki
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

import contracts, nimalyzer, nuklear/nuklear_sdl_renderer
import themes

type
  GameState* = enum
    ## Used to determine the current game's state.
    quitGame, mainMenu, news, allNews, about, showFile, hallOfFame, loadGame,
        loadingGame, newGame, map, endGame, combat, boarding, trade, school,
        recruits, healWounded, repairShip, buyRecipes, shipyard, baseMissions,
        loot, shipInfo, crafting, lastMessages, knowledgeLists, gameStatistics,
        help

  GameDialog* = enum
    ## Used to show any in-game dialog window
    none, loading, questionDialog, gameMenuDialog, errorDialog, waitDialog,
      newGoalDialog, boardingDialog, defendingDialog, recruitDialog,
      negotiateDialog, moduleDialog, missionDialog, acceptMissionDialog,
      renameDialog, giveOrderDialog, memberDialog, renameMemberDialog,
      inventoryDialog, moduleInfoDialog, renameModuleDialog, assignCrewDialog,
      assignAmmoDialog, assignSkillDialog, recipeDialog, setRecipeDialog,
      baseDialog, missionActionDialog, baseActionDialog, ordersDialog,
      destinationDialog, messageDialog, infoDialog, buyDialog, sellDialog,
      takeDialog, dropDialog, moveDialog, giveDialog, dropCargoDialog

const
  dtime*: float = 20.0        ## The length in miliseconds of one game's frame
  menuWidth*: Positive = 600  ## The width of the game's main window
  menuHeight*: Positive = 400 ## The height of the game's main window

var
  fonts*: array[FontsNames, ptr nk_font] = [nil, nil] ## The list of fonts used by the game
  windowWidth*: float = 800.0      ## The width of the main game window
  windowHeight*: float = 600.0     ## The height of the main game window
  dialogX*: float = 0              ## The X position of a dialog
  dialogY*: float = 0              ## The Y position of a dialog
  redraw*: bool = true             ## If true, redraw the game
  inCombat*: bool = false          ## If true, the player is in combat
  theme*: ThemeData = defaultTheme ## The current game's theme
  showOptions*: bool = false       ## If true, show more options in the selected screen
  mapPreview*: bool = false        ## If true, the map is in the preview mode
  previousState*: GameState = map  ## The previous screen in the game
  updateData*: bool = false        ## If true, update the data needed for the selected screen

{.push ruleOff: "varDeclared".}
var images*: array[menuIcon..IconsNames.high,
    PImage] ## The images used in the game
{.pop ruleOn: "varDeclared".}

proc setDialog*(x: float = windowWidth / 3; y: float = windowHeight /
        4) {.raises: [], tags: [], contractual.} =
  ## Set the starting position of a dialog
  ##
  ## * x - the X position of a dialog, can be empty, default to 1/3 of window's
  ##       width
  ## * y - the Y position of a dialog, can be empty, default to 1/4 of window's
  ##       height
  dialogX = x
  dialogY = y
  redraw = true

proc updateDialog*(width, height: float) {.raises: [], tags: [], contractual.} =
  ## Update the current dialog position if needed
  ##
  ## * width  - the dialog width
  ## * height - the dialog height
  if isMouseDown(id = left) and isMouseHovering(rect = Rect(x: dialogX,
      y: dialogY, w: width, h: height)):
    let delta: Vec2 = getMouseDelta()
    dialogX += delta.x
    dialogY += delta.y
