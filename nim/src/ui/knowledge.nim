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
import coreui, errordialog, knowledgebases, knowledgeevents, knowledgemissions,
    knowledgestories, utilsui2

proc showKnowledgeCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Show information about known by player things
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowKnowledge
  var
    knowledgeFrame = mainPaned & ".knowledgeframe"
    knowledgeCanvas = knowledgeFrame & ".bases.canvas"
  if tclEval2(script = "winfo exists " & knowledgeFrame) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "knowledge.tcl")
    var comboValues = " {Any}"
    for baseType in basesTypesList.values:
      comboValues.add(y = " { " & baseType.name & "}")
    var comboBox = knowledgeCanvas & ".frame.options.types"
    tclEval(script = comboBox & " configure -values [list" & comboValues & "]")
    tclEval(script = comboBox & " current 0")
    comboValues = " {Any}"
    comboBox = knowledgeCanvas & ".frame.options.owner"
    for faction in factionsList.values:
      comboValues.add(y = " { " & faction.name & "}")
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
      try:
        finishedStoriesList.add(y = "{ " & storiesList[
            finishedStory.index].name & "}")
      except:
        return showError(message = "Can't get finished story.")
    let optionsFrame = knowledgeFrame & ".options"
    tclEval(script = "ttk::frame " & optionsFrame)
    let storiesBox = optionsFrame & ".titles"
    tclEval(script = "ttk::combobox " & storiesBox &
        " -state readonly -values [list " & finishedStoriesList & "]")
    tclEval(script = "bind " & storiesBox & " <<ComboboxSelected>> ShowStory")
    tclEval(script = storiesBox & " current " & $finishedStories.high)
    tclEval(script = "grid " & storiesBox)
    var button = optionsFrame & ".show"
    tclEval(script = "ttk::button " & button & " -text {Show on map} -command ShowStoryLocation")
    tclEval(script = "grid " & button & " -column 1 -row 0")
    button = optionsFrame & ".set"
    tclEval(script = "ttk::button " & button & " -text {Set as destination for ship} -command SetStory")
    tclEval(script = "grid " & button & " -column 2 -row 0")
    tclEval(script = "grid " & optionsFrame & " -sticky w")
    let storiesView = knowledgeFrame & ".view"
    tclEval(script = "text " & storiesView & " -wrap word")
    tclEval(script = "grid " & storiesView & " -sticky w")
    tclEval(script = "event generate " & storiesBox & " <<ComboboxSelected>>")
  tclEval(script = "update")
  knowledgeCanvas = mainPaned & ".knowledgeframe.stories.canvas"
  tclEval(script = knowledgeCanvas & " configure -scrollregion [list " &
      tclEval2(script = knowledgeCanvas & " bbox all") & "]")
  tclEval(script = knowledgeCanvas & " xview moveto 0.0")
  tclEval(script = knowledgeCanvas & " yview moveto 0.0")
  # Show knowledge
  showScreen(newScreenName = "knowledgeframe")
  return tclOk

proc knowledgeMaxMinCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Maximize or minimize the selected section of knowledge info
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## KnowledgeMaxMin framename
  ## Framename is name of the frame to maximize or minimize
  type FrameInfo = object
    name: string
    column: Natural
    row: Natural
  const frames: array[4, FrameInfo] = [FrameInfo(name: "bases", column: 0,
      row: 0), FrameInfo(name: "missions", column: 0, row: 1), FrameInfo(
      name: "events", column: 1, row: 0), FrameInfo(name: "stories", column: 1, row: 1)]
  let
    frameName = mainPaned & ".knowledgeframe"
    button = frameName & "." & $argv[1] & ".canvas.frame.maxmin"
  if argv[2] == "show":
    for frameInfo in frames:
      let frame = frameName & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame & " -columnspan 2 -rowspan 2 -row 0 -column 0")
      else:
        tclEval(script = "grid remove " & frame)
    tclEval(script = button & " configure -image contracticon -command {KnowledgeMaxMin " &
        $argv[1] & " hide}")
  else:
    for frameInfo in frames:
      let frame = frameName & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame &
            " -columnspan 1 -rowspan 1 -row " & $frameInfo.row & " -column " &
            $frameInfo.column)
      else:
        tclEval(script = "grid " & frame)
    tclEval(script = button & " configure -image expandicon -command {KnowledgeMaxMin " &
        $argv[1] & " show}")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Adds Tcl commands related to the trades UI
  try:
    knowledgebases.addCommands()
    knowledgeevents.addCommands()
    knowledgemissions.addCommands()
    knowledgestories.addCommands()
    addCommand("ShowKnowledge", showKnowledgeCommand)
    addCommand("KnowledgeMaxMin", knowledgeMaxMinCommand)
  except:
    showError(message = "Can't add a Tcl command.")
