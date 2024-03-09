# Copyright 2023-2024 Bartek thindil Jasicki
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

import std/[strutils, tables]
import ../[game, config, crafts, crewinventory, missions, ships, tk, types]
import coreui, dialogs, table

var
  modulesTable: TableWidget
    ## The UI table with all the installed the player's ship's modules
  modulesIndexes: seq[Natural]
    ## The list of indexes of the installed modules

proc showModuleInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let
    moduleIndex = ($argv[1]).parseInt - 1
    moduleDialog = createDialog(name = ".moduledialog",
        title = playerShip.modules[moduleIndex].name, columns = 2)
    moduleCanvas = moduleDialog & ".canvas"
    yScroll = moduleDialog & ".yscroll"
  tclEval(script = "ttk::scrollbar " & yScroll & " -orient vertical -command [list .moduledialog.canvas yview]")
  tclEval(script = "canvas " & moduleCanvas & " -yscrollcommand [list " &
      yScroll & " set]")
  tclEval(script = "grid " & moduleCanvas & " -sticky nwes -padx 5 -pady 5")
  tclEval(script = "grid " & yScroll & " -sticky ns -column 1 -row 1 -padx {0 5} -pady 5")
  tclEval(script = "grid propagate " & moduleDialog & " off")
  tclEval(script = "grid columnconfigure " & moduleDialog & " " & moduleCanvas & " -weight 1")
  tclEval(script = "grid rowconfigure " & moduleDialog & " " & moduleCanvas & " -weight 1")
  tclEval(script = "::autoscroll::autoscroll " & yScroll)
  var
    label = ""
    height: Positive = 10

  proc addLabel(name, labelText: string; row: Natural = 0; column: Natural = 0;
      columnSpan: Natural = 0; wrapLength: Natural = 0;
      countHeight: bool = false; secondary: bool = false) =
    label = name
    tclEval(script = "ttk::label " & label & " -text {" & labelText &
        "} -wraplength " & (if wrapLength > 0: $wrapLength else: "300") & (
        if secondary: " -style Golden.TLabel" else: ""))
    tclEval(script = "grid " & label & " -sticky w -row " & $row & " -column " &
        $column & (if columnSpan > 0: " -columnspan " & $columnSpan else: ""))
    tclEval(script = "SetScrollbarBindings " & label & " " & yScroll)
    if countHeight:
      height = height + tclEval2(script = "winfo reqheight " & label).parseInt

  # Show the module's name
  let
    moduleFrame = moduleCanvas & ".frame"
    module = playerShip.modules[moduleIndex]
  tclEval(script = "ttk::frame " & moduleFrame)
  addLabel(name = moduleFrame & ".nameinfo", labelText = "Name:")
  var currentRow = 0
  addLabel(name = moduleFrame & ".nameinfo2", labelText = module.name,
      row = currentRow, column = 1, secondary = true)
  var infoButton = moduleFrame & ".namebutton"
  let closeDialogButton = moduleFrame & ".button"
  tclEval(script = "ttk::button " & infoButton & " -image editicon -command {" &
      closeDialogButton & " invoke;GetString {Enter a new name for the " &
      module.name & ":} modulename" & $argv[1] & " {Renaming the module} {Rename}} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & infoButton & " \"Set the name for the crew member\"")
  tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky n -padx {5 0}")
  tclEval(script = "bind " & infoButton & " <Escape> {" & closeDialogButton & " invoke; break}")
  tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
  height = height + tclEval2(script = "winfo reqheight " & infoButton).parseInt
  # Show the module's damage
  currentRow.inc
  addLabel(name = moduleFrame & ".damagelbl", labelText = "Status:",
      row = currentRow)
  let damagePercent = (module.durability.float / module.maxDurability.float)
  var progressBarStyle, statusTooltip = ""
  if damagePercent < 1.0 and damagePercent > 0.79:
    progressBarStyle = " -style green.Horizontal.TProgressbar"
    statusTooltip = "Slightly damaged"
  elif damagePercent < 0.8 and damagePercent > 0.49:
    progressBarStyle = " -style yellow.Horizontal.TProgressbar"
    statusTooltip = "Damaged"
  elif damagePercent < 0.5 and damagePercent > 0.19:
    progressBarStyle = " -style yellow.Horizontal.TProgressbar"
    statusTooltip = "Heavily damaged"
  elif damagePercent < 0.2 and damagePercent > 0.0:
    progressBarStyle = ""
    statusTooltip = "Almost destroyed"
  elif damagePercent == 0.0:
    progressBarStyle = ""
    statusTooltip = "Destroyed"
  else:
    progressBarStyle = " -style green.Horizontal.TProgressbar"
    statusTooltip = "Not damaged"
  let moduleMaxValue = (modulesList[module.protoIndex].durability.float * 1.5).Positive
  if module.maxDurability == moduleMaxValue:
    statusTooltip.add(" (max upgrade)")
  let progressBar = moduleFrame & ".damagebar"
  tclEval(script = "ttk::progressbar " & progressBar &
      " -orient horizontal -maximum 1.0 -value {" & $damagePercent & "}" & progressBarStyle)
  tclEval(script = "tooltip::tooltip " & progressBar & " \"" & statusTooltip & "\"")
  tclEval(script = "grid " & progressBar & " -row " & $currentRow & " -column 1 -sticky we")
  infoButton = moduleFrame & ".repairbutton"
  if playerShip.repairModule == moduleIndex:
    tclEval(script = "ttk::button " & infoButton &
        " -image cancelicon -command {" & closeDialogButton & " invoke;SetRepair remove} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Remove the repair priority\"")
  else:
    tclEval(script = "ttk::button " & infoButton &
        " -image repairpriorityicon -command {" & closeDialogButton &
        " invoke;SetRepair assign " & $moduleIndex & "} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Repair selected module as first when damaged\"")
  tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky n -padx {5 0}")
  tclEval(script = "bind " & infoButton & " <Escape> {" & closeDialogButton & " invoke; break}")

  proc addUpgradeButton(upgradeType: ShipUpgrade; buttonTooltip, box: string;
      shipModule: ModuleData; column: Positive = 1;
      buttonName: string = "button"; row: Natural = 0) =
    let upgradeNumber = case upgradeType
      of maxValue:
        "2"
      of value:
        "3"
      else:
        "1"
    infoButton = box & "." & buttonName
    if shipModule.upgradeAction == upgradeType and playerShip.upgradeModule == moduleIndex:
      tclEval(script = "ttk::button " & infoButton &
          " -image cancelicon -command {" & closeDialogButton &
          " invoke;StopUpgrading " & $(moduleIndex + 1) & "} -style Small.TButton")
      tclEval(script = "tooltip::tooltip " & infoButton &
          " \"Stop upgrading the " & buttonTooltip & "\"")
    else:
      tclEval(script = "ttk::button " & infoButton &
          " -image upgradebuttonicon -command {" & closeDialogButton &
          " invoke;SetUpgrade " & upgradeNumber & " " & $(moduleIndex + 1) & "} -style Small.TButton")
      tclEval(script = "tooltip::tooltip " & infoButton &
          " \"Start upgrading the " & buttonTooltip & "\"")
    tclEval(script = "grid " & infoButton & " -row " & $row & " -column " &
        $column & " -sticky n -padx {5 0}")
    tclEval(script = "bind " & infoButton & " <Escape> {" & closeDialogButton & " invoke; break}")

  if module.maxDurability < moduleMaxValue:
    addUpgradeButton(upgradeType = durability,
        buttonTooltip = "module's durability", box = moduleFrame,
        shipModule = module, column = 3, buttonName = "durabilitybutton",
        row = currentRow)
  height = height + tclEval2(script = "winfo reqheight " & infoButton).parseInt
  # Show the module's weight
  currentRow.inc
  addLabel(name = moduleFrame & ".weightlbl", labelText = "Weight: ",
      row = currentRow)
  addLabel(name = moduleFrame & ".weightlbl2", labelText = $module.weight &
      " kg", row = currentRow, column = 1, countHeight = true, secondary = true)
  # Show the module's size
  currentRow.inc
  addLabel(name = moduleFrame & ".lblsize", labelText = "Size: ",
      row = currentRow)
  addLabel(name = moduleFrame & ".lblsize2", labelText = $modulesList[
      module.protoIndex].size, row = currentRow, column = 1, countHeight = true,
      secondary = true)
  # Show the module's repair material
  currentRow.inc
  addLabel(name = moduleFrame & ".lblrepairmaterial",
      labelText = "Repair material: ", row = currentRow, wrapLength = 200)
  let moduleText = moduleFrame & ".info"
  tclEval(script = "text " & moduleText & " -wrap char -height 5 -width 30")
  tclEval(script = moduleText & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  tclEval(script = moduleText & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  var mAmount: Natural = 0
  for item in itemsList.values:
    if item.itemType == modulesList[module.protoIndex].repairMaterial:
      if mAmount > 0:
        tclEval(script = moduleText & " insert end { or }")
      tclEval(script = moduleText & " insert end {" & item.name & "}" & (
          if findItem(inventory = playerShip.cargo, itemType = item.itemType) ==
          -1: " [list red]" else: " [list gold]"))
      mAmount.inc
  tclEval(script = moduleText & " configure -state disabled -height " & $(
      tclEval2(script = moduleText & " count -displaylines 0.0 end").parseInt /
      tclEval2(script = "font metrics InterfaceFont -linespace").parseInt))
  tclEval(script = "grid " & moduleText & " -row " & $currentRow & " -column 1 -sticky nw")
  var newHeight = tclEval2(script = "winfo reqheight " & moduleText).parseInt
  if newHeight < tclEval2(script = "winfo reqheight " & label).parseInt:
    newHeight = tclEval2(script = "winfo reqheight " & label).parseInt
  height = height + newHeight
  # Show the module's upgrade skill
  currentRow.inc
  addLabel(name = moduleFrame & ".upgradeskill", labelText = "Repair skill:",
      row = currentRow, wrapLength = 200, countHeight = true)
  addLabel(name = moduleFrame & ".upgradeskill2", labelText = skillsList[
      modulesList[module.protoIndex].repairSkill].name & "/" & attributesList[
      skillsList[modulesList[module.protoIndex].repairSkill].attribute].name,
      row = currentRow, column = 1, secondary = true)
  # Show the module's upgrade action
  if module.upgradeAction != none:
    currentRow.inc
    var
      moduleInfo = ""
      maxUpgrade = 0
    case module.upgradeAction
    of durability:
      moduleInfo.add("Durability")
      maxUpgrade = modulesList[module.protoIndex].durability
    of maxValue:
      case modulesList[module.protoIndex].mType
      of engine:
        moduleInfo.add("Power")
        maxUpgrade = (modulesList[module.protoIndex].maxValue / 20).int
      of cabin:
        moduleInfo.add("Quality")
        maxUpgrade = modulesList[module.protoIndex].maxValue
      of gun, batteringRam:
        moduleInfo.add("Damage")
        maxUpgrade = modulesList[module.protoIndex].maxValue * 2
      of hull:
        moduleInfo.add("Enlarge")
        maxUpgrade = modulesList[module.protoIndex].maxValue * 40
      of harpoonGun:
        moduleInfo.add("Strength")
        maxUpgrade = modulesList[module.protoIndex].maxValue * 10
      else:
        discard
    of value:
      case modulesList[module.protoIndex].mType:
      of engine:
        moduleInfo.add("Fuel usage")
        maxUpgrade = modulesList[module.protoIndex].value * 20
      else:
        discard
    else:
      discard
    maxUpgrade = (maxUpgrade.float * newGameSettings.upgradeCostBonus).int
    if maxUpgrade == 0:
      maxUpgrade = 1
    let
      upgradePercent = 1.0 - (module.upgradeProgress.float / maxUpgrade.float)
      progressBarStyle = if upgradePercent > 0.74:
          " -style green.Horizontal.TProgressbar"
        elif upgradePercent > 0.24:
          " -style yellow.Horizontal.TProgressbar"
        else:
          " -style Horizontal.TProgressbar"
      progressBar = moduleFrame & ".upgradebar"
    tclEval(script = "ttk::progressbar " & progressBar &
        " -orient horizontal -maximum 1.0 -value {" & $(upgradePercent.float) &
        "}" & progressBarStyle)
    tclEval(script = "tooltip::tooltip " & progressBar & " \"" & moduleInfo & "\"")
    addLabel(name = moduleFrame & ".upgradelbl",
        labelText = "Upgrade progress:", row = currentRow)
    tclEval(script = "grid " & progressBar & " -row " & $currentRow & " -column 1 -sticky we -padx {5 0}")
    if playerShip.upgradeModule == moduleIndex:
      infoButton = moduleFrame & ".upgradebutton"
      tclEval(script = "ttk::button " & infoButton &
          " -image cancelicon -command {" & closeDialogButton &
          " invoke;StopUpgrading " & $argv[1] & "} -style Small.TButton")
      tclEval(script = "tooltip::tooltip " & infoButton & " \"Stop upgrading cabin quality\"")
      tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky n -padx {5 0}")
      tclEval(script = "bind " & infoButton & " <Escape> {" &
          closeDialogButton & " invoke; break}")
    height = height + tclEval2(script = "winfo reqheight " &
        infoButton).parseInt

  proc addOwnersInfo(ownersName: string; addButton: bool = false;
      row: Natural = 0) =
    var ownersText = ownersName
    if module.owner.len > 1:
      ownersText.add("s")
    ownersText.add(" (max " & $module.owner.len & "):")
    addLabel(name = moduleFrame & ".lblowners", labelText = ownersText, row = row)
    ownersText = ""
    var haveOwner = false
    for owner in module.owner:
      if owner > -1:
        if haveOwner:
          ownersText.add(", ")
        haveOwner = true
        ownersText.add(playerShip.crew[owner].name)
    if not haveOwner:
      ownersText.add("none")
    addLabel(name = moduleFrame & ".lblowners2", labelText = ownersText,
        row = row, column = 1, secondary = true)
    if addButton:
      infoButton = moduleFrame & ".ownersbutton"
      tclEval(script = "ttk::button " & infoButton &
          " -image assigncrewicon -command {" & closeDialogButton &
          " invoke;ShowAssignCrew " & $(moduleIndex + 1) & "} -style Small.TButton")
      tclEval(script = "tooltip::tooltip " & infoButton & " \"Assign crew members to the module.\"")
      tclEval(script = "grid " & infoButton & " -row " & $row & " -column 2 -sticky n -padx {5 0}")
      tclEval(script = "bind " & infoButton & " <Escape> {" &
          closeDialogButton & " invoke; break}")
      tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
    height = height + tclEval2(script = "winfo reqheight " &
        infoButton).parseInt


  # Show information specific to the module's type
  case module.mType
  # Show information about engine
  of engine:
    # Show engine power
    currentRow.inc
    var moduleMaxValue = (modulesList[module.protoIndex].maxValue.float * 1.5).int
    addLabel(name = moduleFrame & ".powerlbl", labelText = "Max power: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".powerlbl", labelText = $module.power & (
        if module.power == moduleMaxValue: " (max upgrade)" else: ""),
        row = currentRow, column = 1, secondary = true)
    if module.power < moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue, buttonTooltip = "engine's power",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "powerbutton", row = currentRow)
      height = height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
    else:
      height = height + tclEval2(script = "winfo reqheight " & label).parseInt
    # Show engine fuel usage
    currentRow.inc
    moduleMaxValue = (modulesList[module.protoIndex].value.float / 2.0).int
    addLabel(name = moduleFrame & ".fuellbl", labelText = "Fuel usage: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".fuellbl2", labelText = $module.fuelUsage & (
        if moduleMaxValue == module.fuelUsage: " (max upgrade)" else: ""),
        row = currentRow, column = 1, secondary = true)
    if module.fuelUsage > moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = "engine's fuel usage", box = moduleFrame,
          shipModule = module, column = 2, buttonName = "fuelbutton",
          row = currentRow)
      height = height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
    else:
      height = height + tclEval2(script = "winfo reqheight " & label).parseInt
    # Show engine state
    addLabel(name = moduleFrame & ".statelbl", labelText = "State: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".statelbl2", labelText = (
        if module.disabled: "Disabled" else: "Enabled"), row = currentRow,
        column = 1, secondary = true)
    infoButton = moduleFrame & ".statebutton"
    tclEval(script = "ttk::button " & infoButton &
        " -image powericon -command {" & closeDialogButton &
        " invoke;DisableEngine " & $argv[1] & "} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Turn" & (
        if module.disabled: " on " else: " off ") & "the engine.\"")
    tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky n -padx {5 0}")
    tclEval(script = "bind " & infoButton & " <Escape> {" & closeDialogButton & " invoke; break}")
    height = height + tclEval2(script = "winfo reqheight " &
        infoButton).parseInt
  # Show information about cargo room
  of cargoRoom:
    currentRow.inc
    addLabel(name = moduleFrame & ".maxcargolbl", labelText = "Max cargo: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".maxcargolbl2", labelText = $modulesList[
        module.protoIndex].maxValue & " kg", row = currentRow, column = 1,
        countHeight = true, secondary = true)
  # Show information about hull
  of hull:
    currentRow.inc
    addLabel(name = moduleFrame & ".modules", labelText = "Modules installed: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".modules2",
        labelText = $module.installedModules & " / " & $module.maxModules,
        row = currentRow, column = 1, secondary = true)
    var moduleMaxValue = (modulesList[module.protoIndex].maxValue.float * 1.5).int
    if module.maxModules == moduleMaxValue:
      tclEval(script = label & " configure -text {" & tclEval2(script = label &
          " cget -text") & " (max upgrade)}")
      height = height + tclEval2(script = "winfo reqheight " & label).parseInt
    else:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = "hull's size so it can have more modules installed",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "resizebutton", row = currentRow)
      height = height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
  # Show information about cabin
  of cabin:
    # Show information about cabin's owners
    currentRow.inc
    var isPassenger = false
    block missionLoop:
      for mission in acceptedMissions:
        if mission.mType == passenger:
          for owner in module.owner:
            if mission.data == owner:
              isPassenger = true
              break missionLoop
    addOwnersInfo(ownersName = "Owner", addButton = not isPassenger,
        row = currentRow)
    # Show information about cabin's cleanliness
    currentRow.inc
    addLabel(name = moduleFrame & ".cleanlbl", labelText = "Cleanliness:",
        row = currentRow, countHeight = true)
    var
      damagePercent = 1.0 - (module.cleanliness.float / module.quality.float)
      newStatusTooltip = ""
    if damagePercent == 0.0:
      newStatusTooltip = "Clean"
      progressBarStyle = " -style green.Horizontal.TProgressbar"
    elif damagePercent > 0.0 and damagePercent < 0.2:
      newStatusTooltip = "Bit dusty"
      progressBarStyle = " -style green.Horizontal.TProgressbar"
    elif damagePercent > 0.19 and damagePercent < 0.5:
      newStatusTooltip = "Dusty"
      progressBarStyle = " -style yellow.Horizontal.TProgressbar"
    elif damagePercent > 0.49 and damagePercent < 0.8:
      newStatusTooltip = "Dirty"
      progressBarStyle = " -style yellow.Horizontal.TProgressbar"
    elif damagePercent > 0.79 and damagePercent < 1.0:
      newStatusTooltip = "Very dirty"
      progressBarStyle = ""
    else:
      newStatusTooltip = "Ruined"
      progressBarStyle = ""
    var progressBar = moduleFrame & ".cleanbar"
    tclEval(script = "ttk::progressbar " & progressBar &
        " -orient horizontal -maximum 1.0 -value {" & $(1.0 - damagePercent) &
        "}" & progressBarStyle)
    tclEval(script = "tooltip::tooltip " & progressBar & " \"" &
        newStatusTooltip & "\"")
    tclEval(script = "grid " & progressBar & " -row " & $currentRow & " -column 1 -sticky we")
    # Show information about cabin's quality
    currentRow.inc
    progressBar = moduleFrame & ".qualitybar"
    tclEval(script = "ttk::progressbar " & progressBar &
        " -orient horizontal -maximum 1.0 value {" & $(module.quality.float /
        100.0) & "}")
    addLabel(name = moduleFrame & ".qualitylbl", labelText = "Quality:",
        row = currentRow)
    let moduleMaxValue = (modulesList[module.protoIndex].maxValue.float * 1.5).Positive
    tclEval(script = "tooltip::tooltip " & progressBar & " \"" &
        getCabinQuality(quality = module.quality) & (if module.quality ==
        moduleMaxValue: " (max upgrade)" else: "") & "\"")
    tclEval(script = "grid " & progressBar & " -row " & $currentRow & " -column 1 -sticky we")
    if module.quality < moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = "cabin's quality",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "qualitybutton", row = currentRow)
      height = height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
    else:
      height = height + tclEval2(script = "winfo reqheight " &
          label).parseInt
  # Show information about guns and harpoon guns
  of gun, harpoonGun:
    # Show information about gun's strength
    currentRow.inc
    addLabel(name = moduleFrame & ".strengthlbl", labelText = "Strength: ",
        row = currentRow)
    let
      moduleStrength = (if modulesList[module.protoIndex].mType ==
          ModuleType.gun: module.damage else: module.duration)
      moduleMaxValue = (modulesList[module.protoIndex].maxValue.float * 1.5).Positive
    addLabel(name = moduleFrame & ".strengthlbl2", labelText = $moduleStrength &
        (if moduleStrength == moduleMaxValue: " (max upgrade)" else: ""),
        row = currentRow, column = 1, secondary = true)
    if moduleStrength < moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = (if modulesList[module.protoIndex].mType ==
              ModuleType.gun: "damage" else: "strength") & " of gun",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "strengthbutton", row = currentRow)
      height = height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
    else:
      height = height + tclEval2(script = "winfo reqheight " &
          label).parseInt
    # Show information about gun's owners
    currentRow.inc
    addLabel(name = moduleFrame & ".ammolbl", labelText = "Ammunition:",
        row = currentRow)
    let ammoText = moduleFrame & ".ammoinfo"
    tclEval(script = "text " & ammoText & " -wrap char -height 5 -width 30")
    tclEval(script = ammoText & " tag configure red -foreground " & tclGetVar(
        varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-red)"))
    tclEval(script = ammoText & " tag configure gold -foreground " & tclGetVar(
        varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-goldenyellow)"))
    var haveAmmo = false
    let ammoIndex = (if module.mType == ModuleType2.gun: module.ammoIndex else: module.harpoonIndex)
    if ammoIndex in playerShip.cargo.low .. playerShip.cargo.high and itemsList[
        playerShip.cargo[ammoIndex].protoIndex].itemType == itemsTypesList[
        modulesList[module.protoIndex].value - 1]:
      tclEval(script = ammoText & " insert end {" & itemsList[playerShip.cargo[
          ammoIndex].protoIndex].name & "} [list gold]")
      haveAmmo = true
    if not haveAmmo:
      mAmount = 0
      for index, item in itemsList:
        if item.itemType == itemsTypesList[modulesList[
            module.protoIndex].value - 1]:
          if mAmount > 0:
            tclEval(script = ammoText & " insert end { or }")
          tclEval(script = ammoText & " insert end {" & item.name & "}" & (
              if findItem(inventory = playerShip.cargo, protoIndex = index) >
              -1: "" else: " [list red]"))
          mAmount.inc
    for index, item in playerShip.cargo:
      if itemsList[item.protoIndex].itemType == itemsTypesList[modulesList[
          module.protoIndex].value - 1] and index != ammoIndex:
        infoButton = moduleFrame & ".ammobutton"
        tclEval(script = "ttk::button " & infoButton &
            " -image assignammoicon -command {" & closeDialogButton &
            " invoke;ShowAssignAmmo " & $argv[1] & "} -style Small.TButton")
        tclEval(script = "tooltip::tooltip " & infoButton & " \"Assign an ammo to the gun.\"")
        tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky w -padx {5 0}")
        tclEval(script = "bind " & infoButton & " <Escape> {" &
            closeDialogButton & " invoke; break}")
        tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
    var ammoHeight = (tclEval2(script = ammoText &
        " count -displaylines 0.0 end").parseInt / tclEval2(
        script = "font metrics InterfaceFont -linespace").parseInt).int - 1
    if ammoHeight < 1:
      ammoHeight = 1
    tclEval(script = ammoText & " configure -state disabled -height " & $ammoHeight)
    tclEval(script = "grid " & ammoText & " -sticky w -row " & $currentRow & " -column 1")
    height = height + tclEval2(script = "winfo reqheight " & ammoText).parseInt
    # Show information about gun's fire rate
    if module.mType == ModuleType2.gun:
      currentRow.inc
      addLabel(name = moduleFrame & ".lblfirerate",
          labelText = "Max fire rate: ", row = currentRow)
      addLabel(name = moduleFrame & ".lblfirerate2", labelText = (
          if modulesList[module.protoIndex].speed > 0: $modulesList[
          module.protoIndex].speed & " each turn" else: "1 every " &
          $(modulesList[module.protoIndex].speed.abs) & " turns"),
          row = currentRow, column = 1, countHeight = true, secondary = true)
  # Show information about turrets
  of turret:
    currentRow.inc
    addLabel(name = moduleFrame & ".lblturretgun", labelText = "Weapon:",
        row = currentRow)
    addLabel(name = moduleFrame & "lblturretgun2", labelText = (
        if module.gunIndex > -1: playerShip.modules[
        module.gunIndex].name else: "none"), row = currentRow, column = 1,
        countHeight = true, secondary = true)
  # Show information about workshops
  of workshop:
    # Show information about workshop owners
    currentRow.inc
    addOwnersInfo(ownersName = "Worker", addButton = module.craftingIndex.len >
        0, row = currentRow)
    # Show information about workshop order
    currentRow.inc
    let recipeName = getWorkshopRecipeName(workshop = moduleIndex)
    if recipeName.len > 0:
      addLabel(name = moduleFrame & ".orderlbl", labelText = "Order:",
          row = currentRow)
      addLabel(name = moduleFrame & ".orderlbl2", labelText = recipeName,
          row = currentRow, column = 1, countHeight = true, secondary = true)
      currentRow.inc
      addLabel(name = moduleFrame & ".ordertimelbl",
          labelText = "Finish order in:", row = currentRow)
      addLabel(name = moduleFrame & ".ordertimelbl2",
          labelText = $module.craftingTime & " mins", row = currentRow,
          column = 1, secondary = true)
      infoButton = moduleFrame & ".orderbutton"
      tclEval(script = "ttk::button " & infoButton &
          " -image cancelicon -command {" & closeDialogButton &
          " invoke;CancelOrder " & $argv[1] & "} -style Small.TButton")
      tclEval(script = "tooltip::tooltip " & infoButton & " \"Cancel current crafting order\"")
      tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky w -padx {5 0}")
      tclEval(script = "bind " & infoButton & " <Escape> {" &
          closeDialogButton & " invoke; break}")
      tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
      height = height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
    else:
      addLabel(name = moduleFrame & ".orderlbl", labelText = "Order:",
          row = currentRow)
      addLabel(name = moduleFrame & ".orderlbl2", labelText = "not set",
          row = currentRow, column = 1, countHeight = true, secondary = true)
  # Show information about medical rooms
  of medicalRoom:
    currentRow.inc
    var hasHealingTool = false
    for member in playerShip.crew:
      if member.health < 100 and findItem(inventory = playerShip.cargo,
          itemType = factionsList[playerShip.crew[0].faction].healingTools) > -1:
        hasHealingTool = true
        break
    addOwnersInfo(ownersName = "Medic", addButton = hasHealingTool,
        row = currentRow)
  # Show information about training rooms
  of trainingRoom:
    # Show information about trainees
    currentRow.inc
    addOwnersInfo(ownersName = "Trainee", addButton = module.trainedSkill > 0,
        row = currentRow)
    # Show information about trained skill
    let trainText = (if module.trainedSkill > 0: skillsList[
        module.trainedSkill].name else: "not set")
    currentRow.inc
    addLabel(name = moduleFrame & ".trainlbl", labelText = "Trained skill:",
        row = currentRow)
    addLabel(name = moduleFrame & ".trainlbl2", labelText = trainText,
        row = currentRow, column = 1, secondary = true)
    infoButton = moduleFrame & ".orderbutton"
    tclEval(script = "ttk::button " & infoButton &
        " -image assigncrewicon -command {" & closeDialogButton &
        " invoke;ShowAssignSkill " & $argv[1] & "} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Assign a skill which will be trained in the training room.\"")
    tclEval(script = "grid " & infoButton & " -row " & $currentRow & " -column 2 -sticky w -padx {5 0}")
    tclEval(script = "bind " & infoButton & " <Escape> {" & closeDialogButton & " invoke; break}")
    tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
    height = height + tclEval2(script = "winfo reqheight " &
        infoButton).parseInt
  # Show information about training battering rams
  of batteringRam:
    currentRow.inc
    addLabel(name = moduleFrame & ".strengthlbl", labelText = "Strength:",
        row = currentRow)
    let moduleMaxValue = (modulesList[module.protoIndex].maxValue.float * 1.5).int
    addLabel(name = moduleFrame & ".strengthlbl2", labelText = $module.damage2 &
        (if module.damage2 == moduleMaxValue: " (max upgrade)" else: ""),
        row = currentRow, column = 1, countHeight = true, secondary = true)
    if module.damage2 < moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = "damage of battering ram",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "damagebutton", row = currentRow)
  else:
    discard
  if modulesList[module.protoIndex].description.len > 0:
    currentRow.inc
    tclEval(script = "update")
    addLabel(name = moduleFrame & ".lbldescription", labelText = "\n" &
        modulesList[module.protoIndex].description, row = currentRow,
        countHeight = true, columnSpan = 4, wrapLength = tclEval2(
        script = "winfo reqwidth " & moduleFrame).parseInt)
  addCloseButton(name = moduleFrame & ".button", text = "Close",
      command = "CloseDialog " & moduleDialog, columnSpan = 4,
      row = currentRow + 1)
  tclEval(script = "bind " & closeDialogButton & " <Tab> {focus " &
      moduleFrame & ".nameinfo.button;break}")
  height = height + tclEval2(script = "winfo reqheight " & moduleFrame &
      ".button").parseInt
  if height > 500:
    height = 500
  tclEval(script = moduleCanvas & " create window 0 0 -anchor nw -window " & moduleFrame)
  tclEval(script = moduleCanvas & " configure -scrollregion [list " & tclEval2(
      script = moduleCanvas & " bbox all") & "]")
  height = height + 15 + tclEval2(script = "winfo reqheight " & moduleDialog &
      ".header").parseInt
  tclEval(script = "update")
  let width = tclEval2(script = "winfo reqwidth " & moduleFrame).parseInt +
      tclEval2(script = "winfo reqwidth " & yScroll).parseInt + 5
  tclEval(script = moduleDialog & " configure -height " & $height & " -width " & $width)
  showDialog(dialog = moduleDialog, relativeX = 0.12, relativeY = 0.1)
  return tclOk

proc getModuleInfo(moduleIndex: Natural): string {.sideEffect, raises: [],
    tags: [].} =
  ## Get the additional information about the module
  ##
  ## * moduleIndex - the index of the module in the player's ship to show info
  ##
  ## Returns the string with the additional information about the module or the
  ## empty string if no info is available
  let module = playerShip.modules[moduleIndex]
  case module.mType
  of gun:
    try:
      if module.ammoIndex in 0 ..
          playerShip.cargo.high and itemsList[playerShip.cargo[
              module.ammoIndex].protoIndex].itemType == itemsTypesList[
              modulesList[module.protoIndex].value - 1]:
        result = "Uses " & itemsList[playerShip.cargo[
            module.ammoIndex].protoIndex].name & ", "
      else:
        result = "No ammunition assigned, "
    except:
      result = "No ammunition assigned, "
    if module.owner[0] == -1:
      result.add(" no gunner.")
    else:
      result.add(" " & playerShip.crew[module.owner[0]].name & " is gunner.")
  of workshop:
    let recipeName = try:
        getWorkshopRecipeName(moduleIndex)
      except:
        ""
    if recipeName.len > 0:
      result = recipeName
      var hasWorkers = false
      for owner in module.owner:
        if owner > -1:
          if hasWorkers:
            result.add(", " & playerShip.crew[owner].name)
          else:
            result.add(", workers: " & playerShip.crew[owner].name)
          hasWorkers = true
      if not hasWorkers:
        result.add(", no workers assigned")
      result.add(".")
    else:
      result = "No crafting order."
  of engine:
    if module.disabled:
      result = "Engine disabled"
  of trainingRoom:
    try:
      if module.trainedSkill > 0:
        result = "Set for training " & skillsList[module.trainedSkill].name
        var hasTrainees = false
        for owner in module.owner:
          if owner > -1:
            if hasTrainees:
              result.add(", " & playerShip.crew[owner].name)
            else:
              result.add(", trainees: " & playerShip.crew[owner].name)
            hasTrainees = true
        if not hasTrainees:
          result.add(", no trainees assigned")
        result.add(".")
      else:
        result = "Not set for training."
    except:
      result = "Not set for training."
  else:
    discard

proc updateModulesInfo*(page: Positive = 1) {.sideEffect, raises: [],
    tags: [].} =
  ## Update the list of the player's ship's installed modules
  ##
  ## * page - the number of the current page of the list to show
  let
    shipCanvas = mainPaned & ".shipinfoframe.modules.canvas"
    shipInfoFrame = shipCanvas & ".frame"
  if modulesTable.rowHeight == 0:
    modulesTable = createTable(parent = shipInfoFrame, headers = @["Name",
        "Durability", "Additional info"], scrollbar = mainPaned &
        ".shipinfoframe.modules.scrolly", command = "SortShipModules",
        tooltipText = "Press mouse button to sort the modules.")
  if modulesIndexes.len != playerShip.modules.len:
    modulesIndexes = @[]
    for index, _ in playerShip.modules:
      modulesIndexes.add(index)
  clearTable(modulesTable)
  let startRow = ((page - 1) * gameSettings.listsLimit) + 1
  var
    currentRow = 1
    row = 2
  for index in modulesIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    addButton(table = modulesTable, text = playerShip.modules[index].name,
        tooltip = "Show the module's info", command = "ShowModuleInfo " &
        $(index + 1), column = 1)
    addProgressbar(table = modulesTable, value = playerShip.modules[
        index].durability, maxValue = playerShip.modules[index].maxDurability,
        tooltip = "Show the module's info", command = "ShowModuleInfo " &
        $(index + 1), column = 2)
    addButton(table = modulesTable, text = getModuleInfo(moduleIndex = index),
        tooltip = "Show the module's info", command = "ShowModuleInfo " &
        $(index + 1), column = 3, newRow = true)
    row.inc
    if modulesTable.row == gameSettings.listsLimit + 1:
      break
    if page > 1:
      if modulesTable.row < gameSettings.listsLimit + 1:
        addPagination(table = modulesTable, previousCommand = "ShowModules " &
            $(page - 1))
      else:
        addPagination(table = modulesTable, previousCommand = "ShowModules " &
            $(page - 1), nextCommand = "ShowModules " & $(page + 1))
    elif modulesTable.row == gameSettings.listsLimit + 1:
      addPagination(table = modulesTable, nextCommand = "ShowModules " & $(
          page + 1))
    updateTable(table = modulesTable)
    tclEval(script = "update")
    tclEval(script = shipCanvas & " configure -scrollregion [list " & tclEval2(
        script = shipCanvas & " bbox all") & "]")
    tclEval(script = shipCanvas & " xview moveto 0.0")
    tclEval(script = shipCanvas & " yview moveto 0.0")

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the wait menu
  try:
    addCommand("ShowModuleInfo", showModuleInfoCommand)
  except:
    tclEval(script = "bgerror {Can't add a Tcl command. Reason: " &
        getCurrentExceptionMsg() & "}")

# Temporary code for interfacing with Ada

proc getAdaModuleInfo(moduleIndex: cint): cstring {.raises: [], tags: [], exportc.} =
  return getModuleInfo(moduleIndex - 1).cstring

proc updateAdaModulesInfo(page: cint; mIndexes: array[50, cint];
    columnsWidth: var array[10, cint]) {.raises: [], tags: [], exportc.} =
  modulesIndexes = @[]
  for index in mIndexes:
    if index == 0:
      break
    modulesIndexes.add(index - 1)
  try:
    updateModulesInfo(page)
  except:
    discard
  for index, width in modulesTable.columnsWidth:
    columnsWidth[index] = width.cint

proc addAdaModulesCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
