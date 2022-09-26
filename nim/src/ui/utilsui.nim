# Copyright 2022 Bartek thindil Jasicki
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

{.used.}

import std/strutils
import ../../src/[config, game, tk]
import coreui

proc minutesToDate*(minutes: cint; infoText: var cstring) {.exportc, gcsafe,
    sideEffect, raises: [], tags: [].} =
  ## FUNCTION
  ##
  ## Convert the game minutes to the game time in days, hours, etc
  ##
  ## PARAMETERS
  ##
  ## * minutes  - the amount of minutes to convert
  ## * infoText - the string to which the converted time will be added
  ##
  ## RETURNS
  ##
  ## The updated infoText paramater with converted minutes to the game
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
  ## FUNCTION
  ##
  ## Delete the selected widgets in the selected Tk grid
  ##
  ## PARAMETERS
  ##
  ## * startIndex - The index of the first widget to delete. Starts from 0
  ## * endIndex   - The index of the last widget to delete
  ## * frame      - The parent frame in which the widgets will be deleted
  if endIndex < startIndex:
    return
  let interp = getInterp()
  for i in startIndex .. endIndex:
    if interp.tclEval(script = cstring("grid slaves " & $frame & " -row " &
        $i)) == tclError:
      return
    let tclResult = $interp.tclGetResult()
    for widget in tclResult.split():
      discard interp.tclEval(script = cstring("destroy " & widget))

proc showScreen*(newScreenName: cstring) {.exportc, gcsafe, sideEffect,
    raises: [], tags: [].} =
  const
    paned = mainPaned & ".controls.buttons"
    messagesFrame = mainPaned & ".controls.messages"
  let interp = getInterp()
  if interp.tclEval(script = mainPaned & " panes") == tclError:
    return
  let
    tclResult = $interp.tclGetResult()
    oldSubWindow = tclResult.split()[0]
    subWindow = mainPaned & "." & $newScreenName
  if interp.tclEval(script = cstring(mainPaned & " forget " & oldSubWindow)) == tclError:
    return
  if interp.tclEval(script = cstring(mainPaned & " insert 0 " & subWindow &
      " -weight 1")) == tclError:
    return
  if newScreenName in ["optionsframe".cstring, "messagesframe".cstring] or
      gameSettings.showLastMessages == 0:
    if interp.tclEval(script = cstring("grid remove " & messagesFrame)) == tclError:
      return
    if newScreenName != "mapframe":
      if interp.tclEval(script = cstring("winfo height " & mainPaned)) == tclError:
        return
      let newPos = $interp.tclGetResult()
      if interp.tclEval(script = cstring(mainPaned & " sashpos 0 " & newPos)) == tclError:
        return
  else:
    if oldSubWindow in [mainPaned & ".messagesframe", mainPaned &
        ".optionsframe"]:
      if interp.tclEval(script = cstring(mainPaned & " sashpos 0 " & $(
          gameSettings.windowHeight - gameSettings.messagesPosition))) == tclError:
        return
    if interp.tclEval(script = cstring("grid " & messagesFrame)) == tclError:
      return
  if newScreenName == "mapframe":
    if interp.tclEval(script = cstring("grid " & paned)) == tclError:
      return
  else:
    if interp.tclEval(script = cstring("grid remove " & paned)) == tclError:
      return
