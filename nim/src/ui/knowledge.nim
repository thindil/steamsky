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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

import std/[os, strutils, tables]
import ../[basestypes, game, stories, tk]
import coreui, knowledgebases, knowledgeevents, knowledgemissions, knowledgestories

proc showKnowledgeCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.exportc.} =
  var knowledgeFrame = mainPaned & ".knowledgeframe"
  let knowledgeCanvas = knowledgeFrame & ".bases.canvas"
  if tclEval2(script = "winfo exists " & knowledgeFrame) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "knowledge.tcl")
    var comboValues = " {Any}"
    for baseType in basesTypesList.values:
      comboValues.add(y = "{ " & baseType.name & "}")
    var comboBox = knowledgeCanvas & ".frame.options.types"
    tclEval(script = comboBox & " configure -values [list" & comboValues & "]")
    tclEval(script = comboBox & " current 0")
    comboValues = " {Any}"
    comboBox = knowledgeCanvas & ".frame.options.owner"
    for faction in factionsList.values:
      comboValues.add(y = "{ " & faction.name & "}")
    tclEval(script = comboBox & " configure -values [list" & comboValues & "]")
    tclEval(script = comboBox & " current 0")
  elif tclEval2(script = "winfo ismapped " & knowledgeFrame) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    tclEval(script = "bind . <" & generalAccelerators[0] & "> {}")
    tclEval(script = "bind . <" & generalAccelerators[1] & "> {}")
    tclEval(script = "bind . <" & generalAccelerators[2] & "> {}")
    tclEval(script = "bind . <" & generalAccelerators[3] & "> {}")
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "knowledge")
  tclEval(script = "bind . <" & generalAccelerators[0] & "> {InvokeButton " &
      knowledgeCanvas & ".frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[2] & "> {InvokeButton " &
      knowledgeFrame & ".missions.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[1] & "> {InvokeButton " &
      knowledgeFrame & ".events.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[3] & "> {InvokeButton " &
      knowledgeFrame & ".stories.canvas.frame.maxmin}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  # Setting bases list
  updateBasesList()
  # Setting accepted missions info
  updateMissionsList()
  # Setting the known events list
  updateEventsList()
  # Setting the known stories list
  knowledgeFrame = mainPaned & ".knowledgeframe.stories.canvas.frame"
  var rows = try:
      tclEval2(script = "grid size " & knowledgeFrame).split(" ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = knowledgeFrame)
  if finishedStories.len == 0:
    let label = knowledgeFrame & ".nostories"
    tclEval(script = "ttk::label " & label & " -text {You didn't discover any story yet.} -wraplength 400")
    tclEval(script = "grid " & label & " -padx 10")
  else:
    var finishedStoriesList = ""
    for finishedStory in finishedStories:
      finishedStoriesList.add(y = "{ " & storiesList[finishedStory.index].name & "}")
    let optionsFrame = knowledgeFrame & ".options"
    tclEval(script = "ttk::frame " & optionsFrame)
    let storiesBox = optionsFrame & ".titles"
    tclEval(script = "ttk::combobox " & storiesBox &
        " -state readonly -values [list " & finishedStoriesList & "]")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    knowledgebases.addCommands()
    knowledgeevents.addCommands()
    knowledgemissions.addCommands()
    knowledgestories.addCommands()
#    addCommand("ShowKnowledge", showKnowledgeCommand)
  except:
    showError(message = "Can't add a Tcl command.")
