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

## Provides code related to the player's ship's info screen, like show the
## general information about the ship. Mmoved to separated module to
## avoid circular dependencies.

import std/tables
import contracts, nimalyzer
import ../[config, game, maps, reputation, ships, tk, types]
import coreui, errordialog, shipsuicrew, utilsui2, shipsuimodules2

proc showShipInfoCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], cdecl, contractual, ruleOff: "params".} =
  ## Show information about the player's ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowShipInfo
  var
    shipInfoFrame: string = mainPaned & ".shipinfoframe"
    button: string = mainPaned & ".shipinfoframe.general.canvas.frame.rename"
  if tclEval2(script = "winfo exists " & shipInfoFrame) == "0":
    tclSetVar(varName = "famount", newValue = $(factionsList.len))
    tclEval(script = """
      set shipinfoframe [ttk::frame .gameframe.paned.shipinfoframe]
      # General ship info
      grid [ttk::labelframe $shipinfoframe.general -text {General Info:}] \
         -sticky nwes -padx 4
      set shipcanvas [canvas $shipinfoframe.general.canvas \
         -yscrollcommand [list $shipinfoframe.general.scrolly set] \
         -xscrollcommand [list $shipinfoframe.general.scrollx set]]
      pack [ttk::scrollbar $shipinfoframe.general.scrolly -orient vertical \
         -command [list $shipcanvas yview]] -side right -fill y
      pack [ttk::scrollbar $shipinfoframe.general.scrollx -orient horizontal \
         -command [list $shipcanvas xview]] -fill x -side bottom
      pack $shipcanvas -side top -fill both -expand true
      SetScrollbarBindings $shipcanvas $shipinfoframe.general.scrolly
      ttk::frame $shipcanvas.frame
      grid columnconfigure $shipcanvas.frame 1 -weight 1
      SetScrollbarBindings $shipcanvas.frame $shipinfoframe.general.scrolly
      # Minimize/maximize button
      grid [ttk::frame $shipcanvas.frame.maxmin] -sticky w
      grid [ttk::button $shipcanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {ShipMaxMin general show}] -sticky w \
         -padx 5
      tooltip::tooltip $shipcanvas.frame.maxmin.maxmin \
         {Maximize/minimize the ship general info}
      # Ship name
      grid [ttk::label $shipcanvas.frame.namelbl -text {Name:}] -sticky w -padx 5
      tooltip::tooltip $shipcanvas.frame.namelbl {The name of your ship}
      SetScrollbarBindings $shipcanvas.frame.namelbl $shipinfoframe.general.scrolly
      grid [ttk::label $shipcanvas.frame.name -textvariable shipname \
         -style Golden.TLabel] -column 1 -row 1 -sticky w
      tooltip::tooltip $shipcanvas.frame.name {The name of your ship}
      SetScrollbarBindings $shipcanvas.frame.name $shipinfoframe.general.scrolly
      grid [ttk::button $shipcanvas.frame.rename -style Small.TButton -command {
            GetString {Enter a new name:} shipname {Renaming the ship} {Rename}
         }] -column 2 -row 1 -sticky w
      tooltip::tooltip $shipcanvas.frame.rename {Set a new name for the ship}
      bind $shipcanvas.frame.name <Enter> SetShipName
      # Upgrade progress
      grid [ttk::label $shipcanvas.frame.upgradelabel -text {Upgrade:}] -sticky w \
         -columnspan 3 -padx 5
      SetScrollbarBindings $shipcanvas.frame.upgradelabel \
         $shipinfoframe.general.scrolly
      grid [ttk::label $shipcanvas.frame.upgradelbl -style Golden.TLabel] -column 1 \
         -row 2 -columnspan 2 -sticky w
      SetScrollbarBindings $shipcanvas.frame.upgradelbl \
         $shipinfoframe.general.scrolly
      grid [ttk::progressbar $shipcanvas.frame.upgrade \
         -orient horizontal -maximum 1.0] -sticky we -columnspan 2 -padx 5
      SetScrollbarBindings $shipcanvas.frame.upgrade $shipinfoframe.general.scrolly
      tooltip::tooltip $shipcanvas.frame.upgrade \
         {The current ship's upgrade progress}
      grid [ttk::button $shipcanvas.frame.cancelupgrade -style Small.TButton \
         -command StopUpgrading] -row 3 -column 2 -sticky w
      tooltip::tooltip $shipcanvas.frame.cancelupgrade {Stop the current upgrade}
      # Repair priority
      grid [ttk::label $shipcanvas.frame.repairlabel] -sticky we -padx 5
      tooltip::tooltip $shipcanvas.frame.repairlabel \
         {If damaged, the module will be repaired as the first}
      SetScrollbarBindings $shipcanvas.frame.repairlabel \
         $shipinfoframe.general.scrolly
      grid [ttk::label $shipcanvas.frame.repairlbl -style Golden.TLabel] -column 1 \
         -row 4 -sticky w
      SetScrollbarBindings $shipcanvas.frame.upgradelbl \
         $shipinfoframe.general.scrolly
      tooltip::tooltip $shipcanvas.frame.repairlbl \
         {If damaged, the module will be repaired as the first}
      grid [ttk::button $shipcanvas.frame.cancelpriority -style Small.TButton \
         -command {SetRepair remove}] -row 4 -column 2 -sticky w
      tooltip::tooltip $shipcanvas.frame.cancelpriority {Remove the repair priority}
      # Ship destination
      grid [ttk::label $shipcanvas.frame.destinationlabel] -sticky we -padx 5
      tooltip::tooltip $shipcanvas.frame.destinationlabel \
         {The current travel destination of your ship}
      SetScrollbarBindings $shipcanvas.frame.destinationlabel \
         $shipinfoframe.general.scrolly
      grid [ttk::label $shipcanvas.frame.destinationlbl -style Golden.TLabel] -column 1 \
         -row 5 -sticky w
      tooltip::tooltip $shipcanvas.frame.destinationlbl \
         {The current travel destination of your ship}
      SetScrollbarBindings $shipcanvas.frame.upgradelbl \
         $shipinfoframe.general.scrolly
      grid [ttk::button $shipcanvas.frame.canceldestination -style Small.TButton \
         -command {ResetDestination}] -row 5 -column 2 -sticky w
      tooltip::tooltip $shipcanvas.frame.canceldestination \
         {Reset the ship destination}
      # Ship home base
      grid [ttk::label $shipcanvas.frame.homelabel] -sticky we -padx 5
      tooltip::tooltip $shipcanvas.frame.homelabel {Your ship the current home base}
      SetScrollbarBindings $shipcanvas.frame.homelabel $shipinfoframe.general.scrolly
      grid [ttk::label $shipcanvas.frame.homelbl -style Golden.TLabel] -sticky we \
         -padx 5 -row 6 -column 1
      tooltip::tooltip $shipcanvas.frame.homelbl {Your ship the current home base}
      SetScrollbarBindings $shipcanvas.frame.homelbl $shipinfoframe.general.scrolly
      grid [ttk::button $shipcanvas.frame.showhome -style Small.TButton \
         -command {ShowShipInfo;update;MoveMap centeronhome}] -row 6 -column 2 \
         -sticky w
      tooltip::tooltip $shipcanvas.frame.showhome {Show the home base on map}
      # Ship weight
      grid [ttk::label $shipcanvas.frame.weight] -sticky we -padx 5
      tooltip::tooltip $shipcanvas.frame.weight \
         "The ship weight. The more heavy is ship, the slower it fly\nand need stronger engines"
      SetScrollbarBindings $shipcanvas.frame.weight $shipinfoframe.general.scrolly
      grid [ttk::label $shipcanvas.frame.weight2 -style Golden.TLabel] -columnspan 2 \
         -sticky w -padx 5 -row 7 -column 1
      tooltip::tooltip $shipcanvas.frame.weight2 \
         "The ship weight. The more heavy is ship, the slower it fly\nand need stronger engines"
      SetScrollbarBindings $shipcanvas.frame.weight2 $shipinfoframe.general.scrolly
      # Player's factions' reputation
      grid [ttk::label $shipcanvas.frame.replabel -text {Reputation:}] -sticky we -padx 5 \
        -columnspan 2
      tooltip::tooltip $shipcanvas.frame.replabel {Your reputation among factions}
      SetScrollbarBindings $shipcanvas.frame.replabel $shipinfoframe.general.scrolly
      for {set i 0} {$i < $famount} {incr i} {
        grid [ttk::button $shipcanvas.frame.replbl$i] -sticky w -padx {50 5}
        tooltip::tooltip $shipcanvas.frame.replbl$i {Show information about the faction}
        SetScrollbarBindings $shipcanvas.frame.replbl$i $shipinfoframe.general.scrolly
        grid [ttk::label $shipcanvas.frame.rep$i] -sticky w -padx 5 \
          -row [expr 9 + $i] -column 1
        tooltip::tooltip $shipcanvas.frame.rep$i {Your reputation with the faction}
        SetScrollbarBindings $shipcanvas.frame.rep$i $shipinfoframe.general.scrolly
      }
      $shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
      ::autoscroll::autoscroll $shipinfoframe.general.scrolly
      ::autoscroll::autoscroll $shipinfoframe.general.scrollx
      # Ship modules
      grid [ttk::labelframe $shipinfoframe.modules -text {Modules Info:}] \
         -sticky nwes -padx 4
      set shipcanvas [canvas $shipinfoframe.modules.canvas \
         -yscrollcommand [list $shipinfoframe.modules.scrolly set] \
         -xscrollcommand [list $shipinfoframe.modules.scrollx set]]
      pack [ttk::scrollbar $shipinfoframe.modules.scrolly -orient vertical \
         -command [list $shipcanvas yview]] -side right -fill y
      pack [ttk::scrollbar $shipinfoframe.modules.scrollx -orient horizontal \
         -command [list $shipcanvas xview]] -fill x -side bottom
      pack $shipcanvas -side top -fill both -expand true
      SetScrollbarBindings $shipcanvas $shipinfoframe.modules.scrolly
      ttk::frame $shipcanvas.frame
      $shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
      grid columnconfigure $shipcanvas.frame 1 -weight 1
      SetScrollbarBindings $shipcanvas.frame $shipinfoframe.modules.scrolly
      grid [ttk::frame $shipcanvas.frame.maxmin] -sticky w
      grid [ttk::button $shipcanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {ShipMaxMin modules show}] -sticky w \
         -padx 5
      tooltip::tooltip $shipcanvas.frame.maxmin.maxmin \
         {Maximize/minimize the ship modules info}
      ::autoscroll::autoscroll $shipinfoframe.modules.scrolly
      ::autoscroll::autoscroll $shipinfoframe.modules.scrollx
      # Crew info
      grid [ttk::labelframe $shipinfoframe.crew -text {Crew Info:}] -row 0 -column 1 \
         -sticky nwes -padx 4
      set shipcanvas [canvas $shipinfoframe.crew.canvas \
         -yscrollcommand [list $shipinfoframe.crew.scrolly set] \
         -xscrollcommand [list $shipinfoframe.crew.scrollx set]]
      pack [ttk::scrollbar $shipinfoframe.crew.scrolly -orient vertical \
         -command [list $shipcanvas yview]] -side right -fill y
      pack [ttk::scrollbar $shipinfoframe.crew.scrollx -orient horizontal \
         -command [list $shipcanvas xview]] -fill x -side bottom
      pack $shipcanvas -side top -fill both -expand true
      SetScrollbarBindings $shipcanvas $shipinfoframe.crew.scrolly
      ttk::frame $shipcanvas.frame
      grid columnconfigure $shipcanvas.frame 1 -weight 1
      SetScrollbarBindings $shipcanvas.frame $shipinfoframe.crew.scrolly
      grid [ttk::frame $shipcanvas.frame.maxmin] -sticky w
      grid [ttk::button $shipcanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {ShipMaxMin crew show}] -padx 5
      tooltip::tooltip $shipcanvas.frame.maxmin.maxmin \
         {Maximize/minimize the ship crew info}
      grid [ttk::button $shipcanvas.frame.maxmin.more -style Small.TButton \
         -image moreoptionsicon -command {ShipMore crew show}] -sticky w \
         -row 0 -column 1
      tooltip::tooltip $shipcanvas.frame.maxmin.more \
         {Show/Hide additional options related to managing the crew}
      $shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
      ::autoscroll::autoscroll $shipinfoframe.crew.scrolly
      ::autoscroll::autoscroll $shipinfoframe.crew.scrollx
      # Cargo info
      grid [ttk::labelframe $shipinfoframe.cargo -text {Cargo Info:}] -row 1 \
         -column 1 -sticky nwes -padx 4
      set shipcanvas [canvas $shipinfoframe.cargo.canvas \
         -yscrollcommand [list $shipinfoframe.cargo.scrolly set] \
         -xscrollcommand [list $shipinfoframe.cargo.scrollx set]]
      pack [ttk::scrollbar $shipinfoframe.cargo.scrolly -orient vertical \
         -command [list $shipcanvas yview]] -side right -fill y
      pack [ttk::scrollbar $shipinfoframe.cargo.scrollx -orient horizontal \
         -command [list $shipcanvas xview]] -fill x -side bottom
      pack $shipcanvas -side top -fill both -expand true
      SetScrollbarBindings $shipcanvas $shipinfoframe.cargo.scrolly
      ttk::frame $shipcanvas.frame
      grid columnconfigure $shipcanvas.frame 1 -weight 1
      SetScrollbarBindings $shipcanvas.frame $shipinfoframe.cargo.scrolly
      grid [ttk::frame $shipcanvas.frame.maxmin] -sticky w
      grid [ttk::button $shipcanvas.frame.maxmin.maxmin -style Small.TButton \
         -image expandicon -command {ShipMaxMin cargo show}] -sticky w \
         -padx 5
      tooltip::tooltip $shipcanvas.frame.maxmin.maxmin \
         {Maximize/minimize the ship cargo info}
      grid [ttk::button $shipcanvas.frame.maxmin.more -style Small.TButton \
         -image moreoptionsicon -command {ShipMore cargo show}] -sticky w \
         -row 0 -column 1
      tooltip::tooltip $shipcanvas.frame.maxmin.more \
         {Show/Hide additional options related to managing the cargo}
      grid [ttk::frame $shipcanvas.frame.freeframe] -sticky w -padx 5
      grid [ttk::label $shipcanvas.frame.freeframe.freespace -text {Free cargo space:}] -padx 5
      SetScrollbarBindings $shipcanvas.frame.freeframe.freespace $shipinfoframe.cargo.scrolly
      grid [ttk::label $shipcanvas.frame.freeframe.freespace2 -style Golden.TLabel] \
         -padx 5 -row 0 -column 1
      SetScrollbarBindings $shipcanvas.frame.freeframe.freespace2 $shipinfoframe.cargo.scrolly
      ttk::frame $shipcanvas.frame.selecttype
      grid [ttk::label $shipcanvas.frame.selecttype.label -text {Type:}] -padx 5
      SetScrollbarBindings $shipcanvas.frame.selecttype.label \
         $shipinfoframe.cargo.scrolly
      grid [ttk::combobox $shipcanvas.frame.selecttype.combo -state readonly] -row 0 \
         -column 1
      bind $shipcanvas.frame.selecttype.combo <<ComboboxSelected>> ShowCargo
      tooltip::tooltip $shipcanvas.frame.selecttype \
         {Show only items with the selected type}
      $shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
      ::autoscroll::autoscroll $shipinfoframe.cargo.scrolly
      ::autoscroll::autoscroll $shipinfoframe.cargo.scrollx
      # Configure main ship info grid
      grid columnconfigure $shipinfoframe 0 -weight 1
      grid columnconfigure $shipinfoframe 1 -weight 1
      grid rowconfigure $shipinfoframe 0 -weight 1
      grid rowconfigure $shipinfoframe 1 -weight 1
    """)
    tclEval(script = button & " configure -image editicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.showhome"
    tclEval(script = button & " configure -image showicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.cancelupgrade"
    tclEval(script = button & " configure -image cancelicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.cancelpriority"
    tclEval(script = button & " configure -image cancelicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.canceldestination"
    tclEval(script = button & " configure -image cancelicon")
  elif tclEval2(script = "winfo ismapped " & shipInfoFrame) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    for i in 0 .. 3:
      tclEval(script = "bind . <" & generalAccelerators[i] & "> {}")
    return tclOk
  let shipCanvas: string = shipInfoFrame & ".general.canvas"
  tclEval(script = "bind . <" & generalAccelerators[0] & "> {InvokeButton " &
      shipCanvas & ".frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[2] & "> {InvokeButton " &
      shipInfoFrame & ".modules.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[1] & "> {InvokeButton " &
      shipInfoFrame & ".crew.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[3] & "> {InvokeButton " &
      shipInfoFrame & ".cargo.canvas.frame.maxmin}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  shipInfoFrame = mainPaned & ".shipinfoframe.general.canvas.frame"
  var label: string = shipInfoFrame & ".name"
  tclEval(script = label & " configure -text {Name: " & playerShip.name & "}")
  label = shipInfoFrame & ".upgradelabel"
  let upgradeProgress: string = shipInfoFrame & ".upgrade"
  var cancelButton: string = shipInfoFrame & ".cancelupgrade"
  # Show or hide upgrade module info
  if playerShip.upgradeModule == -1:
    tclEval(script = "grid remove " & label)
    label = shipInfoFrame & ".upgradelbl"
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & upgradeProgress)
    tclEval(script = "grid remove " & cancelButton)
  else:
    var
      upgradeInfo: string = playerShip.modules[
          playerShip.upgradeModule].name & " "
      maxUpgrade: int = 0
    case playerShip.modules[playerShip.upgradeModule].upgradeAction
    of durability:
      upgradeInfo.add(y = "(durability)")
      maxUpgrade = try:
          modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].durability
        except:
          return showError(message = "Can't set max upgrade info.")
    of maxValue:
      try:
        case modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].mType
        of engine:
          upgradeInfo.add(y = "(power)")
          maxUpgrade = (modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue / 20).int
        of cabin:
          upgradeInfo.add(y = "(quality)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue
        of gun, batteringRam:
          upgradeInfo.add(y = "(damage)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 2
        of hull:
          upgradeInfo.add(y = "(enlarge)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 40
        of harpoonGun:
          upgradeInfo.add(y = "(strength)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 10
        else:
          discard
      except:
        return showError(message = "Can't set upgrade info.")
    of value:
      try:
        if modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].mType == engine:
          upgradeInfo.add(y = "(fuel usage)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].value * 20
      except:
        return showError(message = "Can't set upgrade fuel usage info.")
    else:
      discard
    maxUpgrade = (maxUpgrade.float * newGameSettings.upgradeCostBonus).int
    if maxUpgrade == 0:
      maxUpgrade = 1
    let
      upgradePercent: float = 1.0 - (playerShip.modules[
          playerShip.upgradeModule].upgradeProgress.float / maxUpgrade.float)
      progressBarStyle: string = if upgradePercent > 0.74:
          " -style green.Horizontal.TProgressbar"
        elif upgradePercent > 0.24:
          " -style yellow.Horizontal.TProgressbar"
        else:
          " -style Horizontal.TProgressbar"
    tclEval(script = upgradeProgress & " configure -value " & $upgradePercent & progressBarStyle)
    tclEval(script = "grid " & label)
    label = shipInfoFrame & ".upgradelbl"
    tclEval(script = "grid " & label)
    tclEval(script = label & " configure -text {" & upgradeInfo & "}")
    tclEval(script = "grid " & upgradeProgress)
    tclEval(script = "grid " & cancelButton)
  # Show or hide repair priority info
  label = shipInfoFrame & ".repairlabel"
  cancelButton = shipInfoFrame & ".cancelpriority"
  if playerShip.repairModule == -1:
    tclEval(script = "grid remove " & label)
    label = shipInfoFrame & ".repairlbl"
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & cancelButton)
  else:
    tclEval(script = label & " configure -text {Repair first: }")
    tclEval(script = "grid " & label)
    label = shipInfoFrame & ".repairlbl"
    tclEval(script = label & " configure -text {" &
        playerShip.modules[playerShip.repairModule].name & "}")
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & cancelButton)
  # Show or hide destination info
  label = shipInfoFrame & ".destinationlabel"
  cancelButton = shipInfoFrame & ".canceldestination"
  if playerShip.destinationX == 0 and playerShip.destinationY == 0:
    tclEval(script = "grid remove " & label)
    label = shipInfoFrame & ".destinationlbl"
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & cancelButton)
  else:
    if skyMap[playerShip.destinationX][playerShip.destinationY].baseIndex > 0:
      tclEval(script = label & " configure -text {Destination: }")
      let label2: string = shipInfoFrame & ".destinationlbl"
      tclEval(script = label2 & " configure -text {" & skyBases[
          skyMap[playerShip.destinationX][
          playerShip.destinationY].baseIndex].name & "}")
    else:
      tclEval(script = label & " configure -text {Destination: }")
      let label2: string = shipInfoFrame & ".destinationlbl"
      tclEval(script = label2 & " configure -text {X: " &
          $playerShip.destinationX & " Y: " & $playerShip.destinationY & "}")
    tclEval(script = "grid " & label)
    label = shipInfoFrame & ".destinationlbl"
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & cancelButton)
  label = shipInfoFrame & ".homelabel"
  tclEval(script = label & " configure -text {Home: }")
  label = shipInfoFrame & ".homelbl"
  tclEval(script = label & " configure -text {" & skyBases[
      playerShip.homeBase].name & "}")
  label = shipInfoFrame & ".weight"
  tclEval(script = label & " configure -text {Weight: }")
  label = shipInfoFrame & ".weight2"
  try:
    discard tclEval(script = label & " configure -text {" &
        $countShipWeight(ship = playerShip) & "kg}")
  except:
    return showError(message = "Can't show the weight of the ship.")
  # Show player's reputation with factions
  var repIndex: Natural = 0
  for index, faction in factionsList:
    label = shipInfoFrame & ".replbl" & $repIndex
    tclEval(script = label & " configure -text {" & faction.name &
        "} -command {ShowFactionInfo " & index & "}")
    label = shipInfoFrame & ".rep" & $repIndex
    let repLevel: int = getReputation(factionIndex = index)
    tclEval(script = label & " configure -text {" & getReputationText(
        reputationLevel = repLevel) & "} -style " & (if repLevel >
        0: "Headergreen.TLabel" elif repLevel <
        0: "Headerred.TLabel" else: "Golden.TLabel"))
    repIndex.inc
  tclEval(script = "update")
  tclEval(script = shipCanvas & " configure -scrollregion [list " & tclEval2(
      script = shipCanvas & " bbox all") & "]")
  tclEval(script = shipCanvas & " xview moveto 0.0")
  tclEval(script = shipCanvas & " yview moveto 0.0")
  # Setting ship module info
  updateModulesInfo()
  # Setting crew info
  updateCrewInfo()
  # Setting cargo info
  let typeBox: string = mainPaned & ".shipinfoframe.cargo.canvas.frame.selecttype.combo"
  tclEval(script = typeBox & " set All")
  tclEval(script = "event generate " & typeBox & " <<ComboboxSelected>>")
  # Show ship info
  showScreen(newScreenName = "shipinfoframe")
  return tclOk

