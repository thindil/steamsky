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
import ../../src/[game, tk]
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

proc deleteWidgets(startIndex, endIndex: cint; frame: cstring) {.exportc,
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

proc showScreen(newScreenName: cstring) {.exportc.} =
  const
    paned = mainPaned & ".controls.buttons"
    # messageFrame = mainPaned & ".controls.messages"
  let interp = getInterp()
  if interp.tclEval(script = paned & " panes") == tclError:
    return
  let
    tclResult = $interp.tclGetResult()
    oldSubWindow = tclResult.split()[0]
    subWindow = mainPaned & "." & $newScreenName
  if interp.tclEval(script = cstring(mainPaned & " forget " & oldSubWindow)) == tclError:
    return
  if interp.tclEval(script = cstring(mainPaned & " insert 0 " & subWindow & " -weight 1")) == tclError:
    return
#      if New_Screen_Name in "optionsframe" | "messagesframe" or
#        not Game_Settings.Show_Last_Messages then
#         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Messages_Frame);
#         if New_Screen_Name /= "mapframe" then
#            SashPos
#              (Paned => Main_Paned, Index => "0",
#               NewPos => Winfo_Get(Widgt => Main_Paned, Info => "height"));
#         end if;
#      else
#         if Trim
#             (Source => Widget_Image(Win => Old_Sub_Window), Side => Both) in
#             Main_Paned & ".messagesframe" | Main_Paned & ".optionsframe" then
#            SashPos
#              (Paned => Main_Paned, Index => "0",
#               NewPos =>
#                 Natural'Image
#                   (Game_Settings.Window_Height -
#                    Game_Settings.Messages_Position));
#         end if;
#         Tcl.Tk.Ada.Grid.Grid(Slave => Messages_Frame);
#      end if;
#      if New_Screen_Name = "mapframe" then
#         Tcl.Tk.Ada.Grid.Grid(Slave => Paned);
#      else
#         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Paned);
#      end if;
