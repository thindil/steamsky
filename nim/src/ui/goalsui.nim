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

import std/[os, tables, strutils]
import ../[config, game, goals, tk]
import errordialog

proc showGoalsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  tclEvalFile(dataDirectory & "ui" & DirSep & "goals.tcl")
  let
    goalsDialog = ".goalsdialog"
    goalsView = goalsDialog & ".view"
  for goal in goalsList.values:
    tclEval(script = goalsView & " insert " & ($goal.goalType).toUpperAscii &
        " end -id {" & goal.index & "} -text {" & goalText(
        index = goal.index.parseInt) & "}")
  let selectButton = goalsDialog & ".selectbutton"
  tclEval(script = selectButton & " configure -command {SetGoal " & $argv[1] & "}")
  let dialogHeader = goalsDialog & ".header"
  tclEval(script = "bind " & dialogHeader & " <ButtonPress-" & (
      if gameSettings.rightButton: "3" else: "1") & "> {SetMousePosition " &
      dialogHeader & " %X %Y}")
  tclEval(script = "bind " & dialogHeader & " <Motion> {MoveDialog " &
      dialogHeader & " %X %Y}")
  tclEval(script = "bind " & dialogHeader & " <ButtonRelease-" & (
      if gameSettings.rightButton: "3" else: "1") & "> {SetMousePosition " &
      dialogHeader & " 0 0}")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [WriteIOEffect,
    TimeEffect].} =
  ## Adds Tcl commands related to the goals UI
  try:
    discard
#    addCommand("ShowGoals", showGoalsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
