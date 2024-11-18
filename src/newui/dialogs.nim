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

## Provides code related to the game's dialogs, like showing questions, etc.

import std/math
import contracts, nuklear/nuklear_sdl_renderer
import coreui, errordialog

type
  QuestionData = object
    question, data: string
    lines: float

var questionData: QuestionData = QuestionData(question: "", data: "")

proc setQuestion*(question: string; data: string = "", dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data related to the current in-game question
  ##
  ## * question - the question which will be asked to the player
  ## * data     - an additional data for the question, like saved game path,
  ##              optional.
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when an error occurs.
  try:
    var needLines: float = ceil(x = getTextWidth(text = question) / 250)
    if needLines < 1.0:
      needLines = 1.0
    questionData = QuestionData(question: question, data: data, lines: needLines)
  except:
    dialog = setError(message = "Can't set the question.")

proc showQuestion*(dialog: var GameDialog) {.raises: [], tags: [],
    contractual.} =
  ## Show the current question to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player
  ## closed the dialog
  window(name = "Question", x = 150, y = 75, w = 250, h = 150, flags = {
      windowBorder, windowMoveable, windowTitle, windowNoScrollbar}):
    setLayoutRowDynamic(height = 30 * questionData.lines, cols = 1)
    wrapLabel(str = questionData.question)
    setLayoutRowDynamic(height = 30, cols = 2)
    labelButton(title = "Yes"):
      dialog = none
      questionData = QuestionData(question: "", data: "")
    labelButton(title = "No"):
      dialog = none
      questionData = QuestionData(question: "", data: "")
