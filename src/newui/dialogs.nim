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

import std/[os, math]
import contracts, nuklear/nuklear_sdl_renderer
import ../game2
import coreui, errordialog

type
  QuestionType* = enum
    ## Types of questions, used to set actions to the player's response
    deleteSave, showDeadStats, quitGame
  QuestionData = object
    question, data: string
    qType: QuestionType
    lines: float

var
  questionData: QuestionData = QuestionData(question: "", data: "")
  answered*: bool = false ## If true, the question was answered

proc setQuestion*(question: string; qType: QuestionType; data: string = "";
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Set the data related to the current in-game question
  ##
  ## * question - the question which will be asked to the player
  ## * qType    - the type of the question, used to set action to the player's
  ##              answer
  ## * data     - an additional data for the question, like saved game path,
  ##              optional.
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when an error occurs.
  setDialog()
  try:
    var needLines: float = ceil(x = getTextWidth(text = question) / 250)
    if needLines < 1.0:
      needLines = 1.0
    questionData = QuestionData(question: question, data: data,
        lines: needLines, qType: qType)
    answered = false
    dialog = questionDialog
  except:
    dialog = setError(message = "Can't set the question.")

proc showQuestion*(dialog: var GameDialog; state: var GameState) {.raises: [], tags: [RootEffect],
    contractual.} =
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
            dialog = none
          except:
            dialog = setError(message = "Can't remove the save file.")
        of quitGame:
          try:
            endGame(save = true)
            state = mainMenu
            nuklearResizeWin(width = 600, height = 400)
            nuklearSetWindowPos(x = windowCentered, y = windowCentered)
          except:
            dialog = setError(message = "Can't end the game.")
        of showDeadStats:
          discard
      labelButton(title = "No"):
        closePopup()
        dialog = none
      if dialog == none:
        questionData = QuestionData(question: "", data: "")
        answered = true
  except:
    answered = true
    dialog = setError(message = "Can't show the question")
