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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the wait some time menu, like showing the menu,
## executing a wait command, etc.

import contracts, nuklear/nuklear_sdl_renderer
import coreui, errordialog

proc showWaitMenu*(dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the menu with options to wait some in-game time
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened or menu has closed.
  if dialog != waitDialog:
    return
  try:
    const
      height: float = 200
      width: float = 200
    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Wait in place", x = dialogX, y = dialogY,
        w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(30, 1)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")
