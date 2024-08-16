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

import std/[os, tables]
import ../[basestypes, game, tk]
import coreui, knowledgebases, knowledgeevents, knowledgemissions, knowledgestories

proc showKnowledgeCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.exportc.} =
  let
    knowledgeFrame = mainPaned & ".knowledgeframe"
    knowledgeCanvas = knowledgeFrame & ".bases.canvas"
  if tclEval2(script = "winfo exists " & knowledgeFrame) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "knowledge.tcl")
    var comboValues = " {Any}"
    for baseType in basesTypesList.values:
      comboValues.add(y = "{ " & baseType.name & "}")
    let comboBox = knowledgeCanvas & ".frame.options.types"
    tclEval(script = comboBox & " configure -values [list" & comboValues & "]")
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
