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

import std/[colors, math, strutils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, messages, types]
import coreui, errordialog, header, themes

proc showLastMessages*(theme: ThemeData; dialog: var GameDialog;
    inCombat: bool = false; withButtons: bool = true;
    height: float) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the last in-game messages to the player
  ##
  ## * theme       - the current game's theme
  ## * dialog      - the current in-game dialog displayed on the screen
  ## * inCombat    - if true, show messages in combat
  ## * withButtons - if true, show the buttons to resize the last messages
  ##                 window
  ## * height      - the height of the last messages window. If set to 0,
  ##                 it wil be handled outside the procedure.
  ##
  ## Returns parameter dialog, modified if any error happened.
  # Show buttons to resize the last messages window
  if withButtons:
    setLayoutRowDynamic(height = 20, cols = 2)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Make the list of messages bigger.")
    imageButtonCentered(image = images[contract2Icon]):
      gameSettings.messagesPosition += gameSettings.interfaceFontSize + 10
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Make the list of messages smaller.")
    imageButtonCentered(image = images[expand2Icon]):
      gameSettings.messagesPosition -= gameSettings.interfaceFontSize + 10
  var loopStart: int = 0 - messagesAmount()
  if loopStart == 0:
    return
  if loopStart < -10:
    loopStart = -10

  proc showMessage(message: MessageData; dialog: var GameDialog) {.raises: [],
      tags: [RootEffect], contractual.} =
    ## Show the selected message
    ##
    ## * message - the message to show
    let
      colors: array[1..5, Color] = [theme.colors[yellowColor], theme.colors[
          greenColor], theme.colors[redColor], theme.colors[blueColor],
          theme.colors[cyanColor]]
      currentTurnTime: string = "[" & formattedTime() & "]"
      width: float = (if inCombat: windowWidth else: windowWidth * 0.75)
    var needLines: float = try:
          ceil(x = getTextWidth(text = message.message) / width.float)
        except:
          dialog = setError(message = "Can't count the message lenght.")
          return
    if needLines < 1.0:
      needLines = 1.0
    setLayoutRowDynamic(height = 25 * needLines, cols = 1)
    if inCombat:
      if message.message.startsWith(prefix = currentTurnTime):
        if message.color == white:
          wrapLabel(str = message.message)
        else:
          colorWrapLabel(str = message.message, color = colors[
              message.color.ord])
      else:
        colorWrapLabel(str = message.message, color = theme.colors[grayColor])
    else:
      if message.color == white:
        wrapLabel(str = message.message)
      else:
        colorWrapLabel(str = message.message, color = colors[message.color.ord])

  # Show the last messages
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "The last game messages. You can see more of them in Menu->Last messages screen")
  if height > 0:
    setLayoutRowDynamic(height = height, cols = 1)
  group(title = "LastMessagesGroup", flags = {windowBorder}):
    if gameSettings.messagesOrder == olderFirst:
      for i in loopStart .. -1:
        showMessage(message = getMessage(messageIndex = i + 1), dialog = dialog)
    else:
      for i in countdown(a = -1, b = loopStart):
        showMessage(message = getMessage(messageIndex = i + 1), dialog = dialog)

var
  messagesType: Natural = 0
  messageSearch: string = ""

proc showMessages*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the last messages screen
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  const typesList: array[7, string] = ["All", "Combat", "Trade", "Orders", "Craft", "Others", "Missions"]
  setLayoutRowDynamic(height = 35, cols = 3)
  messagesType = comboList(items = typesList, selected = messagesType, itemHeight = 25, x = 400, y = 150)
  editString(text = messageSearch, maxLen = 64)
  labelButton(title = "Delete all messages"):
    discard
