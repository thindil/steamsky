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

## Provides code related to showing in-game messages

import std/[colors, math]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, messages, types]
import coreui, errordialog, themes

proc showLastMessages*(theme: ThemeData; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the last in-game messages to the player
  ##
  ## * theme - the current game's theme
  var loopStart = 0 - messagesAmount()
  if loopStart == 0:
    return
  if loopStart < -10:
    loopStart = -10

  proc showMessage(message: MessageData; dialog: var GameDialog) {.raises: [],
      tags: [RootEffect], contractual.} =
    ## Show the selected message
    ##
    ## * message - the message to show
    let colors: array[1..5, Color] = [theme.colors[32], theme.colors[2],
        theme.colors[28], theme.colors[33], theme.colors[34]]
    var needLines: float = try:
          ceil(x = getTextWidth(text = message.message) / (windowWidth * 0.75).float)
        except:
          dialog = setError(message = "Can't count the message lenght.")
          return
    if needLines < 1.0:
      needLines = 1.0
    setLayoutRowDynamic(height = 25 * needLines, cols = 1)
    if message.color == white:
      wrapLabel(str = message.message)
    else:
      colorWrapLabel(str = message.message, color = colors[message.color.ord])

  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "The last game messages. You can see more of them in Menu->Last messages screen")
  group(title = "LastMessagesGroup", flags = {windowBorder}):
    if gameSettings.messagesOrder == olderFirst:
      for i in loopStart .. -1:
        showMessage(getMessage(messageIndex = i + 1), dialog = dialog)
    else:
      for i in countdown(-1, loopStart):
        showMessage(getMessage(messageIndex = i + 1), dialog = dialog)
