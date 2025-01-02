# Copyright 2024-2025 Bartek thindil Jasicki
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

import std/[tables, strutils]
import contracts, nimalyzer
import ../[config, game, goals, tk, utils]
import errordialog

proc showGoalsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect], contractual, cdecl, ruleOff: "params".} =
  ## Show goals UI to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowGoals buttonpath
  ## Buttonpath is path to the button which is used to set the goal
  tclEval(script = """
      ttk::frame .goalsdialog -style Dialog.TFrame
      set view [ttk::treeview .goalsdialog.view -show tree \
         -yscrollcommand [list .goalsdialog.yscroll set] -height 9 -cursor hand1]
      set selectbutton [ttk::button .goalsdialog.selectbutton -text {Select goal}]
      grid [ttk::label .goalsdialog.header -text {Select a new goal} -wraplength 275 \
         -style Header.TLabel -cursor hand1] -sticky we -columnspan 2
      grid $view -padx 2 -pady {2 0}
      $view column #0 -width 450 -stretch 1
      $view insert {} end -id 0 -text Random
      $view insert {} end -id REPUTATION -text {Gain max reputation in bases}
      $view insert {} end -id DESTROY -text {Destroy enemy ships}
      $view insert {} end -id DISCOVER -text {Discover map}
      $view insert {} end -id VISIT -text {Visit bases}
      $view insert {} end -id CRAFT -text {Craft items}
      $view insert {} end -id MISSION -text {Finish missions}
      $view insert {} end -id KILL -text {Kill enemies in melee combat}
      $view selection set 0
      bind $view <<TreeviewSelect>> {
         set selected [lindex [$view selection] 0]
         if {[$view parent $selected] == {} && \
            [$view item $selected -text] != {Random}} {
            $selectbutton state disabled
         } else {
            $selectbutton state !disabled
         }
      }
      bind $view <Double-1> {$selectbutton invoke}
      bind $view <Return> {$selectbutton invoke}
      grid [ttk::scrollbar .goalsdialog.yscroll -orient vertical \
         -command [list $view yview]] -column 1 -row 1 -sticky ns -padx {0 3} \
         -pady {2 0}
      grid $selectbutton -row 3 -columnspan 2 -sticky we -padx 5 -pady {2 0}
      tooltip::tooltip $selectbutton \
         "Select the goal for your character from the list.\nIf you choose Random option, a random goal will\nbe assigned. You can always change it later during\nthe game, but you will lose all progress then."
      if {[winfo exists .gameframe] && [winfo ismapped .gameframe]} {
         set parent .gameframe
      } else {
         set parent .
      }
      grid [ttk::button .goalsdialog.closebutton -text {Close (Escape)} \
         -command {CloseDialog .goalsdialog $parent}] -row 4 -columnspan 2 \
         -sticky we -padx 5 -pady 2
      tooltip::tooltip .goalsdialog.closebutton \
         {Close the goals list without any changes}
      bind .goalsdialog.closebutton <Escape> {.goalsdialog.closebutton invoke;break}
      bind .goalsdialog.closebutton <Tab> {focus $view;break}
      bind $selectbutton <Escape> {.goalsdialog.closebutton invoke;break}
      bind $view <Escape> {.goalsdialog.closebutton invoke;break}
      ::autoscroll::autoscroll .goalsdialog.yscroll
      place .goalsdialog -in $parent -relx 0.1 -rely 0.075
      focus .goalsdialog.closebutton
      tk busy $parent
      raise .goalsdialog
  """)
  let
    goalsDialog = ".goalsdialog"
    goalsView = goalsDialog & ".view"
  for goal in goalsList.values:
    try:
      tclEval(script = goalsView & " insert " & ($goal.goalType).toUpperAscii &
          " end -id {" & goal.index & "} -text {" & goalText(
          index = goal.index.parseInt) & "}")
    except:
      return showError(message = "Can't add a goal.")
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

proc setGoalCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect], contractual, cdecl.} =
  ## Set selected goal as a current goal
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetGoal buttonpath
  ## Buttonpath is path to the button which is used to set the goal
  let
    goalsView = ".goalsdialog.view"
    selectedGoal = try:
        tclEval2(script = goalsView & " selection").parseInt
      except:
        return showError(message = "Can't get the goal.")
  clearCurrentGoal()
  let buttonName = $argv[1]
  if selectedGoal > 0:
    try:
      currentGoal = goalsList[selectedGoal]
    except:
      return showError(message = "Can't set the current goal.")
  elif "newgamemenu" notin buttonName:
    try:
      currentGoal = goalsList[getRandom(min = 1, max = goalsList.len - 1)]
    except:
      return showError(message = "Can't set random current goal.")
  let goalButton = buttonName
  if selectedGoal > 0:
    var buttonText = try:
        goalText(index = selectedGoal)
      except:
        return showError(message = "Can't get the goal's text.")
    tclEval(script = "tooltip::tooltip " & goalButton & " \"" & buttonText & "\"")
    if buttonText.len > 16:
      buttonText = buttonText[0..16] & "..."
    tclEval(script = goalButton & " configure -text {" & buttonText & "}")
  else:
    tclEval(script = goalButton & " configure -text {Random}")
  tclEval(script = ".goalsdialog.closebutton invoke")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Adds Tcl commands related to the goals UI
  try:
    addCommand("ShowGoals", showGoalsCommand)
    addCommand("SetGoal", setGoalCommand)
  except:
    showError(message = "Can't add a Tcl command.")
