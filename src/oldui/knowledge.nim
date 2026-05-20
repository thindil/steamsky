# Copyright 2024-2026 Bartek thindil Jasicki
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

## Provides code related to the information about the player's character's
## knowledge, like minimizing/maximizing its sections, drawing the UI, etc.

import std/tables
import contracts, nimalyzer
import ../[basestypes, game, tk]
import coreui, errordialog, knowledgebases, knowledgeevents, knowledgemissions,
    knowledgestories, utilsui2

proc showKnowledgeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl,
    contractual, ruleOff: "params".} =
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
    knowledgeFrame: string = mainPaned & ".knowledgeframe"
    knowledgeCanvas: string = knowledgeFrame & ".bases.canvas"
  if tclEval2(script = "winfo exists " & knowledgeFrame) == "0":
    tclEval(script = """
      set knowledgeframe [ttk::frame .gameframe.paned.knowledgeframe]
      # Bases list
      grid [ttk::labelframe $knowledgeframe.bases -text {Known bases:}] -sticky nwes \
         -padx 4
      set knowledgecanvas [canvas $knowledgeframe.bases.canvas \
         -yscrollcommand [list $knowledgeframe.bases.scrolly set] \
         -xscrollcommand [list $knowledgeframe.bases.scrollx set]]
      pack [ttk::scrollbar $knowledgeframe.bases.scrolly -orient vertical \
         -command [list $knowledgecanvas yview]] -side right -fill y
      pack [ttk::scrollbar $knowledgeframe.bases.scrollx -orient horizontal \
         -command [list $knowledgecanvas xview]] -fill x -side bottom
      pack $knowledgecanvas -side top -fill both -expand true
      SetScrollbarBindings $knowledgecanvas $knowledgeframe.bases.scrolly
      ttk::frame $knowledgecanvas.frame
      SetScrollbarBindings $knowledgecanvas.frame $knowledgeframe.bases.scrolly
      # Minimize/maximize button
      grid [ttk::frame $knowledgecanvas.frame.maxmin] -sticky w
      grid [ttk::button $knowledgecanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {KnowledgeMaxMin bases show}] -sticky w \
         -padx 5
      tooltip::tooltip $knowledgecanvas.frame.maxmin.maxmin \
         {Maximize/minimize the list of known bases}
      grid [ttk::button $knowledgecanvas.frame.maxmin.more -style Small.TButton \
         -image moreoptionsicon -command {KnowledgeMore bases show}] -sticky w \
         -row 0 -column 1
      tooltip::tooltip $knowledgecanvas.frame.maxmin.more \
         {Show/Hide additional options related to managing the list of known bases}
      # List of bases options
      ttk::frame $knowledgecanvas.frame.options
      SetScrollbarBindings $knowledgecanvas.frame.options \
         $knowledgeframe.bases.scrolly
      grid [ttk::label $knowledgecanvas.frame.options.typeslbl -text {Type:}]
      SetScrollbarBindings $knowledgecanvas.frame.options.typeslbl \
         $knowledgeframe.bases.scrolly
      grid [ttk::combobox $knowledgecanvas.frame.options.types -state readonly \
         -width 10] -row 0 -column 1
      bind $knowledgecanvas.frame.options.types <<ComboboxSelected>> {ShowBases}
      tooltip::tooltip $knowledgecanvas.frame.options.types \
         {Show only the selected type bases}
      grid [ttk::label $knowledgecanvas.frame.options.statuslbl -text {Status:}] \
         -row 0 -column 2
      SetScrollbarBindings $knowledgecanvas.frame.options.statuslbl \
         $knowledgeframe.bases.scrolly
      grid [ttk::combobox $knowledgecanvas.frame.options.status -state readonly \
         -values [list {Any} {Only not visited} {Only visited}] -width 10] -row 0 \
         -column 3
      bind $knowledgecanvas.frame.options.status <<ComboboxSelected>> {ShowBases}
      tooltip::tooltip $knowledgecanvas.frame.options.status \
         {Show only the selected status bases}
      $knowledgecanvas.frame.options.status current 0
      grid [ttk::label $knowledgecanvas.frame.options.ownerlbl -text {Owner:}] \
         -row 0 -column 4
      SetScrollbarBindings $knowledgecanvas.frame.options.ownerlbl \
         $knowledgeframe.bases.scrolly
      grid [ttk::combobox $knowledgecanvas.frame.options.owner -state readonly \
         -width 10] -row 0 -column 5
      bind $knowledgecanvas.frame.options.owner <<ComboboxSelected>> {ShowBases}
      tooltip::tooltip $knowledgecanvas.frame.options.owner \
         {Show only the selected owner bases}
      grid [ttk::label $knowledgecanvas.frame.options.searchlbl -text {Name:}]
      SetScrollbarBindings $knowledgecanvas.frame.options.searchlbl \
         $knowledgeframe.bases.scrolly
      grid [ttk::entry $knowledgecanvas.frame.options.search -validate key \
         -validatecommand {ShowBases %P} -width 20] -row 1 -column 1 -columnspan 6 \
         -sticky w
      tooltip::tooltip $knowledgecanvas.frame.options.search \
         {Search for a base with the selected name}
      # List of bases
      $knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
      ::autoscroll::autoscroll $knowledgeframe.bases.scrolly
      ::autoscroll::autoscroll $knowledgeframe.bases.scrollx
      # Accepted missions list
      grid [ttk::labelframe $knowledgeframe.missions -text {Accepted missions:}] \
         -sticky nwes -padx 4
      set knowledgecanvas [canvas $knowledgeframe.missions.canvas \
         -yscrollcommand [list $knowledgeframe.missions.scrolly set] \
         -xscrollcommand [list $knowledgeframe.missions.scrollx set]]
      pack [ttk::scrollbar $knowledgeframe.missions.scrolly -orient vertical \
         -command [list $knowledgecanvas yview]] -side right -fill y
      pack [ttk::scrollbar $knowledgeframe.missions.scrollx -orient horizontal \
         -command [list $knowledgecanvas xview]] -fill x -side bottom
      pack $knowledgecanvas -side top -fill both -expand true
      SetScrollbarBindings $knowledgecanvas $knowledgeframe.missions.scrolly
      ttk::frame $knowledgecanvas.frame
      grid columnconfigure $knowledgecanvas.frame 1 -weight 1
      SetScrollbarBindings $knowledgecanvas.frame $knowledgeframe.missions.scrolly
      # Minimize/maximize button
      grid [ttk::frame $knowledgecanvas.frame.maxmin] -sticky w
      grid [ttk::button $knowledgecanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {KnowledgeMaxMin missions show}] \
         -sticky w -padx 5
      tooltip::tooltip $knowledgecanvas.frame.maxmin.maxmin \
         {Maximize/minimize the list of accepted missions}
      $knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
      ::autoscroll::autoscroll $knowledgeframe.missions.scrolly
      ::autoscroll::autoscroll $knowledgeframe.missions.scrollx
      # Known events list
      grid [ttk::labelframe $knowledgeframe.events -text {Known events:}] -row 0 \
         -column 1 -sticky nwes -padx 4
      set knowledgecanvas [canvas $knowledgeframe.events.canvas \
         -yscrollcommand [list $knowledgeframe.events.scrolly set] \
         -xscrollcommand [list $knowledgeframe.events.scrollx set]]
      pack [ttk::scrollbar $knowledgeframe.events.scrolly -orient vertical \
         -command [list $knowledgecanvas yview]] -side right -fill y
      pack [ttk::scrollbar $knowledgeframe.events.scrollx -orient horizontal \
         -command [list $knowledgecanvas xview]] -fill x -side bottom
      pack $knowledgecanvas -side top -fill both -expand true
      SetScrollbarBindings $knowledgecanvas $knowledgeframe.events.scrolly
      ttk::frame $knowledgecanvas.frame
      grid columnconfigure $knowledgecanvas.frame 1 -weight 1
      SetScrollbarBindings $knowledgecanvas.frame $knowledgeframe.events.scrolly
      # Minimize/maximize button
      grid [ttk::frame $knowledgecanvas.frame.maxmin] -sticky w
      grid [ttk::button $knowledgecanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {KnowledgeMaxMin events show}] \
         -sticky w -padx 5
      tooltip::tooltip $knowledgecanvas.frame.maxmin.maxmin \
         {Maximize/minimize the list of known events}
      $knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
      ::autoscroll::autoscroll $knowledgeframe.events.scrolly
      ::autoscroll::autoscroll $knowledgeframe.events.scrollx
      # Known stories list
      grid [ttk::labelframe $knowledgeframe.stories -text {Known stories:}] -row 1 \
         -column 1 -sticky nwes -padx 4
      set knowledgecanvas [canvas $knowledgeframe.stories.canvas \
         -yscrollcommand [list $knowledgeframe.stories.scrolly set] \
         -xscrollcommand [list $knowledgeframe.stories.scrollx set]]
      pack [ttk::scrollbar $knowledgeframe.stories.scrolly -orient vertical \
         -command [list $knowledgecanvas yview]] -side right -fill y
      pack [ttk::scrollbar $knowledgeframe.stories.scrollx -orient horizontal \
         -command [list $knowledgecanvas xview]] -fill x -side bottom
      pack $knowledgecanvas -side top -fill both -expand true
      SetScrollbarBindings $knowledgecanvas $knowledgeframe.stories.scrolly
      ttk::frame $knowledgecanvas.frame
      grid columnconfigure $knowledgecanvas.frame 1 -weight 1
      SetScrollbarBindings $knowledgecanvas.frame $knowledgeframe.stories.scrolly
      # Minimize/maximize button
      grid [ttk::frame $knowledgecanvas.frame.maxmin] -sticky w
      grid [ttk::button $knowledgecanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {KnowledgeMaxMin stories show}] \
         -sticky w -padx 5
      tooltip::tooltip $knowledgecanvas.frame.maxmin.maxmin \
         {Maximize/minimize the list of known stories}
      $knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
      ::autoscroll::autoscroll $knowledgeframe.stories.scrolly
      ::autoscroll::autoscroll $knowledgeframe.stories.scrollx

      # Configure main knowledge info grid
      grid columnconfigure $knowledgeframe 0 -weight 1
      grid columnconfigure $knowledgeframe 1 -weight 1
      grid rowconfigure $knowledgeframe 0 -weight 1
      grid rowconfigure $knowledgeframe 1 -weight 1
    """)
    var comboValues: string = " {Any}"
    for baseType in basesTypesList.values:
      comboValues.add(y = " {" & baseType.name & "}")
    var comboBox: string = knowledgeCanvas & ".frame.options.types"
    tclEval(script = comboBox & " configure -values [list" & comboValues & "]")
    tclEval(script = comboBox & " current 0")
    comboValues = " {Any}"
    comboBox = knowledgeCanvas & ".frame.options.owner"
    for faction in factionsList.values:
      comboValues.add(y = " {" & faction.name & "}")
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
      knowledgeCanvas & ".frame.maxmin.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[2] & "> {InvokeButton " &
      knowledgeFrame & ".missions.canvas.frame.maxmin.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[1] & "> {InvokeButton " &
      knowledgeFrame & ".events.canvas.frame.maxmin.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[3] & "> {InvokeButton " &
      knowledgeFrame & ".stories.canvas.frame.maxmin.maxmin}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  # Setting bases list
  updateBasesList()
  # Setting accepted missions info
  updateMissionsList()
  # Setting the known events list
  updateEventsList()
  # Setting the known stories list
  try:
    updateStoriesList()
  except:
    return showError(message = "Can't show list of stories.")
  # Show knowledge
  showScreen(newScreenName = "knowledgeframe")
  return tclOk

proc knowledgeMaxMinCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
       contractual.} =
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
    frameName: string = mainPaned & ".knowledgeframe"
    button: string = frameName & "." & $argv[1] & ".canvas.frame.maxmin.maxmin"
  if argv[2] == "show":
    for frameInfo in frames:
      let frame: string = frameName & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame & " -columnspan 2 -rowspan 2 -row 0 -column 0")
      else:
        tclEval(script = "grid remove " & frame)
    tclEval(script = button & " configure -image contracticon -command {KnowledgeMaxMin " &
        $argv[1] & " hide}")
  else:
    for frameInfo in frames:
      let frame: string = frameName & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame &
            " -columnspan 1 -rowspan 1 -row " & $frameInfo.row & " -column " &
            $frameInfo.column)
      else:
        tclEval(script = "grid " & frame)
    tclEval(script = button & " configure -image expandicon -command {KnowledgeMaxMin " &
        $argv[1] & " show}")
  return tclOk

proc knowledgeMoreCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Maximize or minimize the selected part in the knowledge info
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShipMore framename show/hide
  ## Framename is name of the frame in which the part will be shown or hidden.
  ## If the second argument is set to show, show the part, otherwise hide it.
  let
    knowledgeFrame: string = mainPaned & ".knowledgeframe"
    button: string = knowledgeFrame & "." & $argv[1] & ".canvas.frame.maxmin.more"
  if argv[1] == "bases":
    if argv[2] == "show":
      tclEval(script = "grid " & knowledgeFrame & ".bases.canvas.frame.options -columnspan 6 -sticky w -padx 5 -row 1")
    else:
      tclEval(script = "grid remove " & knowledgeFrame & ".bases.canvas.frame.options")
  if argv[2] == "show":
    tclEval(script = button & " configure -command {KnowledgeMore " &
        $argv[1] & " hide}")
  else:
    tclEval(script = button & " configure -command {KnowledgeMore " &
        $argv[1] & " show}")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the trades UI
  try:
    knowledgebases.addCommands()
    knowledgeevents.addCommands()
    knowledgemissions.addCommands()
    knowledgestories.addCommands()
    addCommand(name = "ShowKnowledge", nimProc = showKnowledgeCommand)
    addCommand(name = "KnowledgeMaxMin", nimProc = knowledgeMaxMinCommand)
    addCommand(name = "KnowledgeMore", nimProc = knowledgeMoreCommand)
  except:
    showError(message = "Can't add a Tcl command.")
