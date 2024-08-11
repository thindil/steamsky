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
import coreui, dialogs, table

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

var
  missionsTable: TableWidget
  missionsIndexes: seq[Natural]

proc updateMissionsList(page: Positive = 1) =
  if missionsTable.row > 1:
    clearTable(table = missionsTable)
  let
    missionsCanvas = mainPaned & ".knowledgeframe.missions.canvas"
    missionsFrame = missionsCanvas & ".frame"
  var rows = try:
      tclEval2(script = "grid size " & missionsFrame).split(" ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = missionsFrame)
  if acceptedMissions.len == 0:
    let label = missionsFrame & ".nomissions"
    tclEval(script = "ttk::label " & label & " -text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 350")
    tclEval(script = "grid " & label & " -padx 10")
    tclEval(script = "bind " & missionsCanvas & " <Configure> {" & label &
        " configure -wraplength [expr [winfo width " & missionsCanvas & "] - 15]}")
  else:
    tclEval(script = "bind " & missionsCanvas & " <Configure> {}")
    missionsTable = createTable(parent = missionsFrame, headers = @["Name",
        "Distance", "Coordinates", "Details", "Time limit", "Base reward"],
        scrollbar = ".gameframe.paned.knowledgeframe.missions.scrolly",
        command = "SortAccepted_Missions",
        tooltipText = "Press mouse button to sort the missions.")
    var row = 2

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the accepted missions UI
  try:
    discard
#    addCommand("ShowMissionMenu", showMissionsMenuCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc updateAdaMissionsList(page: cint) {.sideEffect, raises: [],
    tags: [RootEffect], exportc.} =
  try:
    updateMissionsList(page = page.Positive)
  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace(getCurrentException())
