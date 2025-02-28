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

var
  waitAmount: Positive = 1
  waitInterval: Natural = 0

proc showWaitMenu*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the menu with options to wait some in-game time
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened or menu has closed.
  if dialog != waitDialog:
    return
  try:
    const
      height: float = 320
      width: float = 400
    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Wait in place", x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(30, 1)
      labelButton(title = "Wait 1 minute"):
        discard
      labelButton(title = "Wait 5 minutes"):
        discard
      labelButton(title = "Wait 10 minutes"):
        discard
      labelButton(title = "Wait 15 minutes"):
        discard
      labelButton(title = "Wait 30 minutes"):
        discard
      labelButton(title = "Wait 1 hour"):
        discard
      setLayoutRowDynamic(30, 3)
      labelButton(title = "Wait"):
        discard
      let newValue: int = property2(name = "#", min = 1, val = waitAmount,
          max = 1440, step = 1, incPerPixel = 1)
      if newValue != waitAmount:
        waitAmount = newValue
      var newInterval: Natural = comboList(items = ["minutes", "hours", "days"],
          selected = waitInterval, itemHeight = 25, x = 200, y = 180)
      if newInterval != waitInterval:
        waitInterval = newInterval
      setLayoutRowDynamic(30, 1)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")
