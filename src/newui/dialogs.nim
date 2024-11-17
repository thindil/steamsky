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

import contracts
import coreui

type
  QuestionData = object
    question, data: string

var questionData: QuestionData = QuestionData(question: "", data: "")

proc setQuestion*(question: string; data: string = "") {.raises: [], tags: [], contractual.} =
  ## Set the data related to the current in-game question
  ##
  ## * question - the question which will be asked to the player
  ## * data     - an additional data for the question, like saved game path,
  ##              optional.
  questionData = QuestionData(question: question, data: data)

proc showQuestion*(dialog: var GameDialog) {.raises: [], tags: [], contractual.} =
  dialog = questionDialog
