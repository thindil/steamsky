# Copyright 2025 Bartek thindil Jasicki
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

## Provides code related to the information about the list of known stories,
## like sorting them, showing information about them, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../[game, messages, stories, types]
import coreui, dialogs, errordialog, mapsui, setui

var storyIndex: Natural = 0

proc showStoriesInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the known stories
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # No stories discovered
  if knownStoriesList.len == 0:
    setLayoutRowDynamic(height = 100, cols = 1)
    wrapLabel(str = "You didn't discover any story yet.")
  else:
    setLayoutRowStatic(height = 30, cols = 3, ratio = [200.cfloat, 150, 250])
    let newStoryIndex = comboList(items = knownStoriesList,
        selected = storyIndex, itemHeight = 25, x = 150, y = 200)
    if newStoryIndex != storyIndex:
      storyIndex = newStoryIndex
    labelButton(title = "Show on map"):
      var (newX, newY) = try:
          getStoryLocation()
        except:
          dialog = setError(message = "Can't get the story location.")
          return
      centerX = newX
      centerY = newY
      mapPreview = true
    labelButton(title = "Set as destination for ship"):
      var (newX, newY) = try:
          getStoryLocation()
        except:
          dialog = setError(message = "Can't get the story location.")
          return
      if newX == playerShip.skyX and newY == playerShip.skyY:
        dialog = setMessage(message = "You are at this location now.",
            title = "Can't set destination")
        return
      playerShip.destinationX = newX
      playerShip.destinationY = newY
      addMessage(message = "You set the travel destination for your ship.",
          mType = orderMessage)
