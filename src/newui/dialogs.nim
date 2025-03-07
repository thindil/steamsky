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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's dialogs, like showing questions, etc.

import std/[colors, os, math, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, game2, shipscrew2]
import coreui, errordialog, themes

type
  QuestionType* = enum
    ## Types of questions, used to set actions to the player's response
    deleteSave, showDeadStats, quitGame, resignGame
  QuestionData = object
    question, data: string
    qType: QuestionType
    lines: float
  MessageData = object
    text, title: string
    lines: float
  ButtonSettings* = object
    ## Used to store information about a button in a dialog
    text*: string    ## Text to show on the button
    code*: proc(dialog: var GameDialog) ## The code to execute when the button was pressed
    icon*: int       ## The index of the icon to show on the button
    tooltip*: string ## The tooltip text associated with the button
    color*: string   ## The color of the button's text
  TextData = object
    text: string
    lines: float
    widgets: Positive = 1
    color: Color
  InfoData = object
    data: seq[TextData]
    title: string
    button1, button2: ButtonSettings

const emptyButtonSettings*: ButtonSettings = ButtonSettings(text: "", code: nil,
    icon: -1, tooltip: "",
    color: "") ## Empty Button setting, used to disable the selected button

var
  questionData: QuestionData = QuestionData(question: "", data: "")
  messageData: MessageData = MessageData(text: "", title: "Info")
  answered*: bool = false ## If true, the question was answered
  infoData: InfoData = InfoData(data: @[], title: "",
      button1: emptyButtonSettings, button2: emptyButtonSettings)

proc setQuestion*(question: string; qType: QuestionType;
    data: string = ""): GameDialog {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data related to the current in-game question
  ##
  ## * question - the question which will be asked to the player
  ## * qType    - the type of the question, used to set action to the player's
  ##              answer
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the questionDialog if the message was set, otherwise
  ## errorDialog
  setDialog()
  try:
    var needLines: float = ceil(x = getTextWidth(text = question) / 250)
    if needLines < 1.0:
      needLines = 1.0
    questionData = QuestionData(question: question, data: data,
        lines: needLines, qType: qType)
    answered = false
    result = questionDialog
  except:
    result = setError(message = "Can't set the question.")

proc showQuestion*(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the current question to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current game's state
  ##
  ## Returns the parameter dialog and state. The first is modified only when
  ## the player closed the dialog and the second when the player quit the game.
  if dialog != questionDialog:
    return
  try:
    const
      width: float = 250
      height: float = 150

    proc setMainMenu(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [], contractual.} =
      ## Set the main window for the main game menu
      ##
      ## * dialog - the current in-game dialog displayed on the screen
      ## * state  - the current game's state
      ##
      ## Returns the parameter dialog and state.
      state = endGame
      nuklearResizeWin(width = menuWidth, height = menuHeight)
      nuklearSetWindowPos(x = windowCentered, y = windowCentered)
      nuklearSetWindowResizable(resizable = false)
      closePopup()
      dialog = none

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Question", x = dialogX, y = dialogY,
        w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30 * questionData.lines, cols = 1)
      wrapLabel(str = questionData.question)
      setLayoutRowDynamic(height = 30, cols = 2)
      labelButton(title = "Yes"):
        closePopup()
        case questionData.qType
        of deleteSave:
          try:
            removeFile(file = questionData.data)
            closePopup()
            dialog = none
          except:
            dialog = setError(message = "Can't remove the save file.")
        of quitGame:
          try:
            endGame(save = true)
            setMainMenu(dialog = dialog, state = state)
          except:
            dialog = setError(message = "Can't end the game.")
        of resignGame:
          try:
            death(memberIndex = 0, reason = "resignation", ship = playerShip)
            closePopup()
            dialog = none
          except:
            dialog = setError(message = "Can't kill the player.")
        of showDeadStats:
          discard
      labelButton(title = "No"):
        if questionData.qType == showDeadStats:
          setMainMenu(dialog = dialog, state = state)
        else:
          closePopup()
          dialog = none
      if dialog == none:
        questionData = QuestionData(question: "", data: "")
        answered = true
  except:
    answered = true
    dialog = setError(message = "Can't show the question")

proc setMessage*(message, title: string): GameDialog {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the data related to the current in-game message
  ##
  ## * message - the message which will be show to the player
  ## * title   - the title of the message's dialog
  ##
  ## Returns the messageDialog if the message was set, otherwise
  ## errorDialog
  setDialog()
  try:
    var needLines: float = ceil(x = getTextWidth(text = message) / 250)
    if needLines < 1.0:
      needLines = 1.0
    messageData = MessageData(text: message, title: title, lines: needLines)
    result = messageDialog
  except:
    result = setError(message = "Can't set the message.")

proc showMessage*(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the current question to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if dialog != messageDialog:
    return
  try:
    const
      width: float = 250
      height: float = 150

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = messageData.title, x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30 * messageData.lines, cols = 1)
      wrapLabel(str = messageData.text)
      setLayoutRowDynamic(height = 30, cols = 2)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the message")

proc setInfo*(text, title: string; button1: ButtonSettings = emptyButtonSettings;
    button2: ButtonSettings = emptyButtonSettings): GameDialog {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the data related to the current in-game info dialog
  ##
  ## * text    - the text to show in the dialog. Can use special tags for
  ##             colors, like `{gold}{/gold}`
  ## * title   - the title of the dialog
  ## * button1 - the settings for the first optional button. If empty, the
  ##             button will not show
  ## * button2 - the settings for the second optional button. If empty, the
  ##             button will not show
  ##
  ## Returns the infoDialog if the info was set, otherwise errorDialog
  setDialog()
  try:
    const width: float = 250
    var
      startIndex: int = 0
      tagIndex: int = text.find(sub = '{')
      parts: seq[TextData] = @[]
    let
      theme: ThemeData = try:
          themesList[gameSettings.interfaceTheme]
        except:
          return
    while true:
      if tagIndex == -1:
        tagIndex = text.len
      var
        partText: string = text[startIndex..tagIndex - 1]
        needLines: float = ceil(x = getTextWidth(text = partText) / width)
      if needLines < 1.0:
        needLines = 1.0
      parts.add(y = TextData(text: partText, color: theme.colors[foregroundColor], lines: needLines, widgets: 1))
      if tagIndex == text.len:
        break
      startIndex = tagIndex
      tagIndex = text.find(sub = '}', start = startIndex)
      let tagName: string = text[startIndex + 1..tagIndex - 1]
      startIndex = tagIndex + 1
      tagIndex = text.find(sub = "{/" & tagName & "}", start = startIndex)
      partText = text[startIndex..tagIndex - 1]
      needLines = ceil(x = getTextWidth(text = partText) / width)
      if needLines < 1.0:
        needLines = 1.0
      parts.add(y = TextData(text: partText, color: case tagName
        of "gold":
          theme.colors[goldenColor]
        of "green":
          theme.colors[greenColor]
        of "red:":
          theme.colors[redColor]
        else:
          theme.colors[foregroundColor], lines: needLines, widgets: 1))
      startIndex = tagIndex + tagName.len + 3
      tagIndex = text.find(sub = '{', start = startIndex)
    var lineWidth, wAmount: Natural = 0
    for part in parts.mitems:
      if part.lines > 1:
        lineWidth = 0
        wAmount = 0
        continue
      lineWidth += getTextWidth(text = part.text).Natural
      if lineWidth <= width.Natural:
        wAmount.inc
      else:
        part.widgets = wAmount
        wAmount = 0
        lineWidth = 0
    infoData = InfoData(data: parts, button1: button1, button2: button2)
    result = infoDialog
  except:
    result = setError(message = "Can't set the message.")

proc showInfo*(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the info to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if dialog != infoDialog:
    return
  try:
    const
      width: float = 250
      height: float = 150

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = infoData.title, x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30 * messageData.lines, cols = 1)
      wrapLabel(str = infoData.data[0].text)
      setLayoutRowDynamic(height = 30, cols = 2)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the info")
