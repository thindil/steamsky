# Copyright 2022-2023 Bartek thindil Jasicki
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

import std/strutils
import ../../src/[config, tk, types]
import coreui

type AddingCommandError* = object of CatchableError
  ## Raised when there is problem with adding a Tcl command

proc addCommand*(name: string; nimProc: TclCmdProc) {.sideEffect, raises: [
    AddingCommandError], tags: [].} =
  ## Add the selected Nim procedure as a Tcl command.
  ##
  ## * name    - the name of the Tcl command
  ## * nimProc - the Nim procedure which will be executed as the Tcl command
  ##
  ## Raises AddingCommandError exception if the command can't be added.
  if tclEval2(script = "info commands " & name).len > 0:
    raise newException(exceptn = AddingCommandError,
        message = "Command with name " & name & " exists.")
  if tclCreateCommand(interp = getInterp(), cmdName = name.cstring,
      cproc = nimProc, clientData = 0, deleteProc = nil) == nil:
    raise newException(exceptn = AddingCommandError,
        message = "Can't add command " & name)

proc minutesToDate*(minutes: cint; infoText: var cstring) {.exportc, gcsafe,
    sideEffect, raises: [], tags: [].} =
  ## Convert the game minutes to the game time in days, hours, etc
  ##
  ## * minutes  - the amount of minutes to convert
  ## * infoText - the string to which the converted time will be added
  ##
  ## Returns the updated infoText paramater with converted minutes to the game
  ## time
  var
    travelTime: DateRecord
    minutesDiff: int = minutes
  while minutesDiff > 0:
    if minutesDiff > 518_400:
      minutesDiff = minutesDiff - 518_400
      travelTime.year.inc()
    elif minutesDiff in 43_201 .. 518_400:
      minutesDiff = minutesDiff - 43_200
      travelTime.month.inc()
      if travelTime.month > 12:
        travelTime.year.inc()
        travelTime.month = 1
    elif minutesDiff in 1_441..43_200:
      minutesDiff = minutesDiff - 1_440
      travelTime.day.inc()
      if travelTime.day > 31:
        travelTime.month.inc()
        if travelTime.month > 12:
          travelTime.year.inc()
          travelTime.month = 1
    elif minutesDiff in 61..1_440:
      minutesDiff = minutesDiff - 60
      travelTime.hour.inc()
      if travelTime.hour > 23:
        travelTime.hour = 0
        travelTime.day.inc()
        if travelTime.day > 31:
          travelTime.day = 1
          travelTime.month.inc()
          if travelTime.month > 12:
            travelTime.month = 1
            travelTime.year.inc()
    else:
      travelTime.minutes = minutesDiff
      minutesDiff = 0
    if travelTime.year == 4_000_000:
      break
  var timeText: string = $infoText
  if travelTime.year > 0:
    timeText = timeText & " " & $travelTime.year & "y"
  if travelTime.month > 0:
    timeText = timeText & " " & $travelTime.month & "m"
  if travelTime.day > 0:
    timeText = timeText & " " & $travelTime.day & "d"
  if travelTime.hour > 0:
    timeText = timeText & " " & $travelTime.hour & "h"
  if travelTime.minutes > 0:
    timeText = timeText & " " & $travelTime.minutes & "mins"
  infoText = timeText.cstring

proc deleteWidgets*(startIndex, endIndex: cint; frame: cstring) {.exportc,
    gcsafe, sideEffect, raises: [], tags: [].} =
  ## Delete the selected widgets in the selected Tk grid
  ##
  ## * startIndex - The index of the first widget to delete. Starts from 0
  ## * endIndex   - The index of the last widget to delete
  ## * frame      - The parent frame in which the widgets will be deleted
  if endIndex < startIndex:
    return
  let interp = getInterp()
  for i in startIndex .. endIndex:
    if tclEval(script = "grid slaves " & $frame & " -row " & $i) == tclError:
      return
    let tclResult = $interp.tclGetResult()
    for widget in tclResult.split():
      tclEval(script = "destroy " & widget)

proc showScreen*(newScreenName: cstring) {.exportc, sideEffect,
    raises: [], tags: [].} =
  ## Clear the old screen and show the selected to the player
  ##
  ## * newScreenName - the Tcl name of the screen which will be show
  const
    paned = mainPaned & ".controls.buttons"
    messagesFrame = mainPaned & ".controls.messages"
  let interp = getInterp()
  if tclEval(script = mainPaned & " panes") == tclError:
    return
  let
    tclResult = $interp.tclGetResult()
    oldSubWindow = tclResult.split()[0]
    subWindow = mainPaned & "." & $newScreenName
  if tclEval(script = mainPaned & " forget " & oldSubWindow) == tclError:
    return
  if tclEval(script = mainPaned & " insert 0 " & subWindow &
      " -weight 1") == tclError:
    return
  if newScreenName in ["optionsframe".cstring, "messagesframe"] or
      not gameSettings.showLastMessages:
    if tclEval(script = "grid remove " & messagesFrame) == tclError:
      return
    if newScreenName != "mapframe":
      if tclEval(script = "winfo height " & mainPaned) == tclError:
        return
      let newPos = $interp.tclGetResult()
      if tclEval(script = mainPaned & " sashpos 0 " & newPos) == tclError:
        return
  else:
    if oldSubWindow in [mainPaned & ".messagesframe", mainPaned &
        ".optionsframe"]:
      if tclEval(script = mainPaned & " sashpos 0 " & $(
          gameSettings.windowHeight - gameSettings.messagesPosition)) == tclError:
        return
    if tclEval(script = "grid " & messagesFrame) == tclError:
      return
  if newScreenName == "mapframe":
    if tclEval(script = "grid " & paned) == tclError:
      return
  else:
    if tclEval(script = "grid remove " & paned) == tclError:
      return

proc resizeCanvasCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  let canvas = $argv[1]
  if tclEval2(script = "winfo exists " & canvas) == "0":
    return tclOk
  let parentFrame = tclEval2(script = "winfo parent " & canvas)
  tclEval(script = "bind " & parentFrame & " <Configure>")
  tclEval(script = canvas & " configure -width " & $argv[2] &
      " -height [expr " & $argv[3] & " - 20]")
  tclEval(script = "bind " & parentFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  return tclOk

proc addCommands*() =
  addCommand("ResizeCanvas", resizeCanvasCommand)

# Temporary code for interfacing with Ada

proc addAdaUtilsCommands() {.raises: [], tags: [], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
