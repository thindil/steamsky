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

import std/strutils
import ../[missions, tk]
import dialogs

proc showMissionsMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let
    missionIndex = ($argv[1]).parseInt - 1
    acceptedMission = acceptedMissions[missionIndex]
    missionMenu = createDialog(name = ".missionslistmenu", title = (
      case acceptedMission.mType
      of deliver:
        "Deliver item"
      of destroy:
        "Destroy enemy"
      of patrol:
        "Patrol area"
      of explore:
        "Explore area"
      of passenger:
        "Transport passenger") & " mission actions", parentName = ".", columns = 3)

  proc addButton(name, label, command, icon, tooltipText: string;
      column: Natural; color: string = "") =
    let button = missionMenu & name
    tclEval(script = "ttk::button " & button & " -text {" & label &
        "} -command {CloseDialog " & missionMenu & " .;" & command &
        "} -image " & icon & " -style Dialog" & color & ".TButton")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the accepted missions UI
  try:
    discard
#    addCommand("ShowMissionMenu", showMissionsMenuCommand)
  except:
    showError(message = "Can't add a Tcl command.")
