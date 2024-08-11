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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the menu with available the selected mission options
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMissionMenu missionindex
  ## MissionIndex is the index of the mission's menu to show
  let
    missionIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the mission's index.")
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
        "} -image " & icon & "icon -style Dialog" & color & ".TButton")
    tclEval(script = "grid " & button & " -sticky we -padx 5 -pady {0 5} -row 1 -column " & $column)
    tclEval(script = "tooltip::tooltip " & button & " \"" & tooltipText & "\"")
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " &
        missionMenu & " .;break}")
    if name == ".show":
      tclEval(script = "bind " & button & " <Tab> {focus " & missionMenu & ".destination;break}")
      tclEval(script = "focus " & missionMenu & ".destination;break")

  addButton(name = ".destination", tooltipText = "Set the mission as destination for the ship.",
      command = "SetDestination2 " & $acceptedMission.targetX & " " &
      $acceptedMission.targetY, icon = "destination", label = "Target",
      column = 0, color = "green")
  addButton(name = ".close", label = "Close", command = "", icon = "exit",
      tooltipText = "Close the dialog.", column = 1)
  addButton(name = ".show", tooltipText = "Show the mission on map.",
      command = "ShowOnMap " & $acceptedMission.targetX & " " &
      $acceptedMission.targetY, icon = "show2", label = "Show", column = 2,
      color = "green")
  showDialog(dialog = missionMenu, parentFrame = ".")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the accepted missions UI
  try:
    discard
#    addCommand("ShowMissionMenu", showMissionsMenuCommand)
  except:
    showError(message = "Can't add a Tcl command.")
