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
import ../[game, config, crafts, crewinventory, items, messages, missions,
    ships, shipscargo, shipscrew, shipsupgrade, tk, types]
import dialogs, updateheader, shipsuicrew, table, utilsui2

proc showModuleInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Show information about the selected module and set option for it
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowModuleInfo moduleindex
  ## ModuleIndex is the index of the module to show
  let
    moduleIndex = try:
        ($argv[1]).parseInt - 1
      except:
        tclEval(script = "bgerror {Can't get module index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
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
      countHeight: bool = false; secondary: bool = false) {.sideEffect,
          raises: [], tags: [].} =
    label = name
    tclEval(script = "ttk::label " & label & " -text {" & labelText &
        "} -wraplength " & (if wrapLength > 0: $wrapLength else: "300") & (
        if secondary: " -style Golden.TLabel" else: ""))
    tclEval(script = "grid " & label & " -sticky w -row " & $row & " -column " &
        $column & (if columnSpan > 0: " -columnspan " & $columnSpan else: ""))
    tclEval(script = "SetScrollbarBindings " & label & " " & yScroll)
    if countHeight:
      height = try:
          height + tclEval2(script = "winfo reqheight " & label).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the label. Reason: " &
              getCurrentExceptionMsg() & "}")
          return

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
  height = try:
      height + tclEval2(script = "winfo reqheight " & infoButton).parseInt
    except:
      tclEval(script = "bgerror {Can't count the height of the button. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
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
  let moduleMaxValue = try:
      (modulesList[module.protoIndex].durability.float * 1.5).Positive
    except:
      tclEval(script = "bgerror {Can't count the module's max value. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
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
  height = try:
      height + tclEval2(script = "winfo reqheight " & infoButton).parseInt
    except:
      tclEval(script = "bgerror {Can't count the height of the button (2). Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
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
  try:
    addLabel(name = moduleFrame & ".lblsize2", labelText = $modulesList[
        module.protoIndex].size, row = currentRow, column = 1,
        countHeight = true,
        secondary = true)
  except:
    tclEval(script = "bgerror {Can't show the label. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
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
    try:
      if item.itemType == modulesList[module.protoIndex].repairMaterial:
        if mAmount > 0:
          tclEval(script = moduleText & " insert end { or }")
        tclEval(script = moduleText & " insert end {" & item.name & "}" & (
            if findItem(inventory = playerShip.cargo,
                itemType = item.itemType) ==
            -1: " [list red]" else: " [list gold]"))
        mAmount.inc
    except:
      tclEval(script = "bgerror {Can't show repair material. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  try:
    discard tclEval(script = moduleText &
        " configure -state disabled -height " & $(tclEval2(script = moduleText &
            " count -displaylines 0.0 end").parseInt /
        tclEval2(script = "font metrics InterfaceFont -linespace").parseInt))
  except:
    tclEval(script = "bgerror {Can't configure moduleText. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  tclEval(script = "grid " & moduleText & " -row " & $currentRow & " -column 1 -sticky nw")
  var newHeight = try:
      tclEval2(script = "winfo reqheight " & moduleText).parseInt
    except:
      tclEval(script = "bgerror {Can't count the height of the text. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  try:
    if newHeight < tclEval2(script = "winfo reqheight " & label).parseInt:
      newHeight = tclEval2(script = "winfo reqheight " & label).parseInt
  except:
    tclEval(script = "bgerror {Can't count the new height of the text. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  height = height + newHeight
  # Show the module's upgrade skill
  currentRow.inc
  addLabel(name = moduleFrame & ".upgradeskill", labelText = "Repair skill:",
      row = currentRow, wrapLength = 200, countHeight = true)
  try:
    addLabel(name = moduleFrame & ".upgradeskill2", labelText = skillsList[
        modulesList[module.protoIndex].repairSkill].name & "/" & attributesList[
        skillsList[modulesList[module.protoIndex].repairSkill].attribute].name,
        row = currentRow, column = 1, secondary = true)
  except:
    tclEval(script = "bgerror {Can't show the upgrade skill. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  # Show the module's upgrade action
  if module.upgradeAction != none:
    currentRow.inc
    var
      moduleInfo = ""
      maxUpgrade = 0
    case module.upgradeAction
    of durability:
      moduleInfo.add("Durability")
      maxUpgrade = try:
          modulesList[module.protoIndex].durability
      except:
        tclEval(script = "bgerror {Can't get max upgrade. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    of maxValue:
      try:
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
      except:
        tclEval(script = "bgerror {Can't show info about upgrade. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    of value:
      try:
        case modulesList[module.protoIndex].mType:
        of engine:
          moduleInfo.add("Fuel usage")
          maxUpgrade = modulesList[module.protoIndex].value * 20
        else:
          discard
      except:
        tclEval(script = "bgerror {Can't show info about fuel usage upgrade. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
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
    height = try:
        height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the button (3). Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk

  proc addOwnersInfo(ownersName: string; addButton: bool = false;
      row: Natural = 0) {.sideEffect, raises: [], tags: [].} =
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
    height = try:
        height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the crew button. Reason: " &
            getCurrentExceptionMsg() & "}")
        return


  # Show information specific to the module's type
  case module.mType
  # Show information about engine
  of engine:
    # Show engine power
    currentRow.inc
    var moduleMaxValue = try:
        (modulesList[module.protoIndex].maxValue.float * 1.5).int
      except:
        tclEval(script = "bgerror {Can't count the module max value. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    addLabel(name = moduleFrame & ".powerlbl", labelText = "Max power: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".powerlbl2", labelText = $module.power & (
        if module.power == moduleMaxValue: " (max upgrade)" else: ""),
        row = currentRow, column = 1, secondary = true)
    if module.power < moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue, buttonTooltip = "engine's power",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "powerbutton", row = currentRow)
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            infoButton).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the button (4). Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    else:
      height = try:
          height + tclEval2(script = "winfo reqheight " & label).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the button (5). Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    # Show engine fuel usage
    currentRow.inc
    moduleMaxValue = try:
          (modulesList[module.protoIndex].value.float / 2.0).int
      except:
        tclEval(script = "bgerror {Can't count the module's max value (2). Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
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
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            infoButton).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the button (6). Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    else:
      height = try:
          height + tclEval2(script = "winfo reqheight " & label).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the label. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    # Show engine state
    currentRow.inc
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
    height = try:
        height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the button (7). Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  # Show information about cargo room
  of cargoRoom:
    currentRow.inc
    addLabel(name = moduleFrame & ".maxcargolbl", labelText = "Max cargo: ",
        row = currentRow)
    try:
      addLabel(name = moduleFrame & ".maxcargolbl2", labelText = $modulesList[
          module.protoIndex].maxValue & " kg", row = currentRow, column = 1,
          countHeight = true, secondary = true)
    except:
      tclEval(script = "bgerror {Can't show the max cargo. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  # Show information about hull
  of hull:
    currentRow.inc
    addLabel(name = moduleFrame & ".modules", labelText = "Modules installed: ",
        row = currentRow)
    addLabel(name = moduleFrame & ".modules2",
        labelText = $module.installedModules & " / " & $module.maxModules,
        row = currentRow, column = 1, secondary = true)
    var moduleMaxValue = try:
        (modulesList[module.protoIndex].maxValue.float * 1.5).int
      except:
        tclEval(script = "bgerror {Can't count the module's max value (3). Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    if module.maxModules == moduleMaxValue:
      tclEval(script = label & " configure -text {" & tclEval2(script = label &
          " cget -text") & " (max upgrade)}")
      height = try:
          height + tclEval2(script = "winfo reqheight " & label).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the label (3). Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    else:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = "hull's size so it can have more modules installed",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "resizebutton", row = currentRow)
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            infoButton).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the button (8). Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
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
        " -orient horizontal -style blue.Horizontal.TProgressbar -maximum 1.0 -value {" &
        $(module.quality.float / 100.0) & "}")
    addLabel(name = moduleFrame & ".qualitylbl", labelText = "Quality:",
        row = currentRow)
    let moduleMaxValue = try:
          (modulesList[module.protoIndex].maxValue.float * 1.5).Positive
      except:
        tclEval(script = "bgerror {Can't count the cabin's max value. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    tclEval(script = "tooltip::tooltip " & progressBar & " \"" &
        getCabinQuality(quality = module.quality) & (if module.quality ==
        moduleMaxValue: " (max upgrade)" else: "") & "\"")
    tclEval(script = "grid " & progressBar & " -row " & $currentRow & " -column 1 -sticky we")
    if module.quality < moduleMaxValue:
      addUpgradeButton(upgradeType = maxValue,
          buttonTooltip = "cabin's quality",
          box = moduleFrame, shipModule = module, column = 2,
          buttonName = "qualitybutton", row = currentRow)
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            infoButton).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the cabin's button. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    else:
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            label).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the cabin's label. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  # Show information about guns and harpoon guns
  of gun, harpoonGun:
    # Show information about gun's strength
    currentRow.inc
    addLabel(name = moduleFrame & ".strengthlbl", labelText = "Strength: ",
        row = currentRow)
    let
      moduleStrength = try:
          (if modulesList[module.protoIndex].mType ==
            ModuleType.gun: module.damage else: module.duration)
        except:
          tclEval(script = "bgerror {Can't count the module's strength. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
      moduleMaxValue = try:
            (modulesList[module.protoIndex].maxValue.float * 1.5).Positive
        except:
          tclEval(script = "bgerror {Can't count the gun's max value. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    addLabel(name = moduleFrame & ".strengthlbl2", labelText = $moduleStrength &
        (if moduleStrength == moduleMaxValue: " (max upgrade)" else: ""),
        row = currentRow, column = 1, secondary = true)
    if moduleStrength < moduleMaxValue:
      try:
        addUpgradeButton(upgradeType = maxValue,
            buttonTooltip = (if modulesList[module.protoIndex].mType ==
                ModuleType.gun: "damage" else: "strength") & " of gun",
            box = moduleFrame, shipModule = module, column = 2,
            buttonName = "strengthbutton", row = currentRow)
        height = height + tclEval2(script = "winfo reqheight " &
            infoButton).parseInt
      except:
        tclEval(script = "bgerror {Can't show the gun's upgrade button. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    else:
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            label).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the gun's button. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    # Show information about gun's owners
    currentRow.inc
    addOwnersInfo(ownersName = "Gunner", addButton = true, row = currentRow)
    # Show information about gun's ammunition
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
    try:
      if ammoIndex in playerShip.cargo.low .. playerShip.cargo.high and
          itemsList[playerShip.cargo[ammoIndex].protoIndex].itemType ==
              itemsTypesList[
          modulesList[module.protoIndex].value - 1]:
        tclEval(script = ammoText & " insert end {" & itemsList[
            playerShip.cargo[ammoIndex].protoIndex].name & "} [list gold]")
        haveAmmo = true
    except:
      tclEval(script = "bgerror {Can't check for the ammo. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
    if not haveAmmo:
      mAmount = 0
      for index, item in itemsList:
        try:
          if item.itemType == itemsTypesList[modulesList[
              module.protoIndex].value - 1]:
            if mAmount > 0:
              tclEval(script = ammoText & " insert end { or } [list gold]")
            tclEval(script = ammoText & " insert end {" & item.name & "}" & (
                if findItem(inventory = playerShip.cargo, protoIndex = index) >
                -1: " [list gold]" else: " [list red]"))
            mAmount.inc
        except:
          tclEval(script = "bgerror {Can't find ammo. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    for index, item in playerShip.cargo:
      try:
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
      except:
        tclEval(script = "bgerror {Can't set gun's ammo button. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    var ammoHeight = try:
          (tclEval2(script = ammoText &
            " count -displaylines 0.0 end").parseInt / tclEval2(
            script = "font metrics InterfaceFont -linespace").parseInt).int - 1
      except:
        tclEval(script = "bgerror {Can't count the height of the ammo text. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    if ammoHeight < 1:
      ammoHeight = 1
    tclEval(script = ammoText & " configure -state disabled -height " & $ammoHeight)
    tclEval(script = "grid " & ammoText & " -sticky w -row " & $currentRow & " -column 1")
    height = try:
        height + tclEval2(script = "winfo reqheight " & ammoText).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the ammo text (2). Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    # Show information about gun's fire rate
    if module.mType == ModuleType2.gun:
      currentRow.inc
      addLabel(name = moduleFrame & ".lblfirerate",
          labelText = "Max fire rate: ", row = currentRow)
      try:
        addLabel(name = moduleFrame & ".lblfirerate2", labelText = (
            if modulesList[module.protoIndex].speed > 0: $modulesList[
            module.protoIndex].speed & " each turn" else: "1 every " &
            $(modulesList[module.protoIndex].speed.abs) & " turns"),
            row = currentRow, column = 1, countHeight = true, secondary = true)
      except:
        tclEval(script = "bgerror {Can't show the info about fire rate. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  # Show information about turrets
  of turret:
    currentRow.inc
    addLabel(name = moduleFrame & ".lblturretgun", labelText = "Weapon:",
        row = currentRow)
    addLabel(name = moduleFrame & ".lblturretgun2", labelText = (
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
    let recipeName = try:
        getWorkshopRecipeName(workshop = moduleIndex)
      except:
        tclEval(script = "bgerror {Can't get the recipe name. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
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
      height = try:
          height + tclEval2(script = "winfo reqheight " &
            infoButton).parseInt
        except:
          tclEval(script = "bgerror {Can't count the height of the workshop's button. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
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
      try:
        if member.health < 100 and findItem(inventory = playerShip.cargo,
            itemType = factionsList[playerShip.crew[0].faction].healingTools) > -1:
          hasHealingTool = true
          break
      except:
        tclEval(script = "bgerror {Can't find wounded crew members. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    addOwnersInfo(ownersName = "Medic", addButton = hasHealingTool,
        row = currentRow)
  # Show information about training rooms
  of trainingRoom:
    # Show information about trainees
    currentRow.inc
    addOwnersInfo(ownersName = "Trainee", addButton = module.trainedSkill > 0,
        row = currentRow)
    # Show information about trained skill
    let trainText = try:
        (if module.trainedSkill > 0: skillsList[
          module.trainedSkill].name else: "not set")
      except:
        tclEval(script = "bgerror {Can't set trainText. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
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
    height = try:
        height + tclEval2(script = "winfo reqheight " &
          infoButton).parseInt
      except:
        tclEval(script = "bgerror {Can't count the height of the train button. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  # Show information about training battering rams
  of batteringRam:
    currentRow.inc
    addLabel(name = moduleFrame & ".strengthlbl", labelText = "Strength:",
        row = currentRow)
    let moduleMaxValue = try:
        (modulesList[module.protoIndex].maxValue.float * 1.5).int
      except:
        tclEval(script = "bgerror {Can't count the battering ram max value. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
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
  try:
    if modulesList[module.protoIndex].description.len > 0:
      currentRow.inc
      tclEval(script = "update")
      addLabel(name = moduleFrame & ".lbldescription", labelText = "\n" &
          modulesList[module.protoIndex].description, row = currentRow,
          countHeight = true, columnSpan = 4, wrapLength = tclEval2(
          script = "winfo reqwidth " & moduleFrame).parseInt)
  except:
    tclEval(script = "bgerror {Can't show the description. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  addCloseButton(name = moduleFrame & ".button", text = "Close",
      command = "CloseDialog " & moduleDialog, columnSpan = 4,
      row = currentRow + 1)
  tclEval(script = "bind " & closeDialogButton & " <Tab> {focus " &
      moduleFrame & ".nameinfo.button;break}")
  height = try:
      height + tclEval2(script = "winfo reqheight " & moduleFrame &
        ".button").parseInt
    except:
      tclEval(script = "bgerror {Can't count the height of the button (10). Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  if height > 500:
    height = 500
  tclEval(script = moduleCanvas & " create window 0 0 -anchor nw -window " & moduleFrame)
  tclEval(script = moduleCanvas & " configure -scrollregion [list " & tclEval2(
      script = moduleCanvas & " bbox all") & "]")
  height = try:
      height + 15 + tclEval2(script = "winfo reqheight " & moduleDialog &
        ".header").parseInt
    except:
      tclEval(script = "bgerror {Can't count the height of the dialog. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  tclEval(script = "update")
  let width = try:
        tclEval2(script = "winfo reqwidth " & moduleFrame).parseInt +
          tclEval2(script = "winfo reqwidth " & yScroll).parseInt + 5
      except:
        tclEval(script = "bgerror {Can't count the width of the dialog. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  tclEval(script = moduleDialog & " configure -height " & $height & " -width " & $width)
  showDialog(dialog = moduleDialog, relativeX = 0.12, relativeY = 0.1)
  return tclOk

proc setUpgradeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].}
  ## Set the selected upgrade for the selected module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetUpgrade upgradetype moduleindex
  ## upgradetype is type of upgrade to start: 1, 2 or 3. moduleindex is the
  ## index of the player ship module which will be upgraded

proc assignModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].}
  ## Assign member, ammo or skill to module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## AssignModule assigntype moduleindex assignindex
  ## assigntype is type of item to assing to module: crew, ammo, skills.
  ## moduleindex is the index of the Player_Ship module to which item will be
  ## assigned. assignindex is the index of the item which will be assigned
  ## to the module

proc disableEngineCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].}
  ## Enable or disable selected engine
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DisableEngine engineindex
  ## engineindex is the index of the engine module in the player ship

proc stopUpgradingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].}
  ## Stop the current ship upgrade
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## StopUpgrading

proc setRepairCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Set or remove the repair priority from the selected module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetRepair action
  ## Action can be assing or remove. If assing, then assing the currently
  ## selected module as the repair first, otherwise clear current priority
  ## setting

proc resetDestinationCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Reset the current destination point for the player's ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ResetDestination

proc updateAssignCrewCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].} =
  ## Update assign the crew member UI
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateAssignCrew moduleindex ?crewindex?
  ## Moduleindex is the index of the module to which a new crew members will
  ## be assigned. Crewindex is the index of the crew member which will be
  ## assigned or removed
  let
    moduleIndex = try:
        ($argv[1]).parseInt - 1
      except:
        tclEval(script = "bgerror {Can't get the module index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    frameName = ".moduledialog.canvas.frame"
    crewIndex = try:
        (if argc == 3: ($argv[2]).parseInt else: -1)
      except:
        tclEval(script = "bgerror {Can't get the crew index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  if argc == 3:
    if tclGetVar(varName = frameName & ".crewbutton" & $argv[2]) == "0":
      for owner in playerShip.modules[moduleIndex].owner.mitems:
        if owner == crewIndex:
          owner = -1
          break
      try:
        if modulesList[playerShip.modules[moduleIndex].protoIndex].mType !=
            ModuleType.cabin:
          giveOrders(ship = playerShip, memberIndex = crewIndex,
              givenOrder = rest, moduleIndex = -1, checkPriorities = false)
      except CrewOrderError, CrewNoSpaceError:
        showMessage(text = getCurrentExceptionMsg(),
            title = "Can't give a order")
        return tclOk
      except:
        tclEval(script = "bgerror {Can't give order to a crew member. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    elif assignModuleCommand(clientData = clientData, interp = interp, argc = 4,
        argv = ["assignModule".cstring, "crew", argv[1], argv[2]]) != tclOk:
      return tclError
  for index, _ in playerShip.crew:
    let crewButton = frameName & ".crewbutton" & $index
    tclEval(script = crewButton & " state !disabled")
    tclEval(script = crewButton & " configure -takefocus 1")
  var assigned = 0
  for owner in playerShip.modules[moduleIndex].owner:
    if owner > -1:
      assigned.inc
  if assigned == playerShip.modules[moduleIndex].owner.len:
    for index, _ in playerShip.crew:
      let crewButton = frameName & ".crewbutton" & $index
      if tclGetVar(varName = crewButton) == "0":
        tclEval(script = crewButton & " state disabled")
        tclEval(script = crewButton & " configure -takefocus 0")
  let infoLabel = frameName & ".infolabel"
  if tclEval2(script = "winfo exists " & infoLabel) == "1":
    tclEval(script = infoLabel & " configure -text {Available: " &
        $(playerShip.modules[moduleIndex].owner.len - assigned) & "}")
    updateHeader()
    updateCrewInfo()
  return tclOk

proc showAssignCrewCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].} =
  ## Show assign the crew member UI
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowAssignCrew moduleindex
  ## Moduleindex is the index of the module to which a new crew members will
  ## be assigned.
  let
    moduleIndex = try:
        ($argv[1]).parseInt - 1
      except:
        tclEval(script = "bgerror {Can't get the module index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    module = playerShip.modules[moduleIndex]
    moduleDialog = createDialog(name = ".moduledialog",
        title = "Assign a crew member to " & module.name, titleWidth = 250)
    yScroll = moduleDialog & ".yscroll"
    crewCanvas = moduleDialog & ".canvas"
  tclEval(script = "ttk::scrollbar " & yScroll & " -orient vertical -command [list .moduledialog.canvas yview]")
  tclEval(script = "canvas " & crewCanvas & " -yscrollcommand [list " &
      yScroll & " set]")
  tclEval(script = "grid " & crewCanvas & " -sticky nwes -padx 5 -pady 5")
  tclEval(script = "grid " & yScroll & " -sticky ns -padx {0 5} -pady {5 0} -row 0 -column 1")
  let closeButton = moduleDialog & ".button"
  tclEval(script = "ttk::button " & closeButton &
      "  -text Close -command {CloseDialog " & moduleDialog & "}")
  tclEval(script = "grid " & closeButton & " -pady {0 5} -columnspan 2")
  tclEval(script = "focus " & closeButton)
  tclEval(script = "::autoscroll::autoscroll " & yScroll)
  let
    crewFrame = crewCanvas & ".frame"
    recipe = if module.mType == ModuleType2.workshop:
        try:
          setRecipeData(recipeIndex = module.craftingIndex)
        except:
          tclEval(script = "bgerror {Can't set the recipe. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
      else:
        CraftData()
  var
    height = 10
    width = 250
    assigned = 0
  tclEval(script = "ttk::frame " & crewFrame)
  for index, member in playerShip.crew:
    let crewButton = crewFrame & ".crewbutton" & $index
    tclEval(script = "ttk::checkbutton " & crewButton & " -text {" &
        member.name & (if module.mType == ModuleType2.workshop: getSkillMarks(
        skillIndex = recipe.skill, memberIndex = index) else: "") &
        "} -command {UpdateAssignCrew " & $argv[1] & " " & $index & "}")
    tclSetVar(varName = crewButton, newValue = "0")
    for owner in module.owner:
      if owner == index:
        tclSetVar(varName = crewButton, newValue = "1")
        assigned.inc
        break
    tclEval(script = "pack " & crewButton & " -anchor w")
    height = try:
          height + tclEval2(script = "winfo reqheight " & crewButton).parseInt
      except:
        tclEval(script = "bgerror {Can't set the height. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    try:
      if tclEval2(script = "winfo reqwidth " & crewButton).parseInt + 10 > width:
        width = tclEval2(script = "winfo reqwidth " & crewButton).parseInt + 10
    except:
      tclEval(script = "bgerror {Can't set the width. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
    tclEval(script = "bind " & crewButton & " <Escape> {" & closeButton & " invoke;break}")
    tclEval(script = "bind " & crewButton & " <Tab> {focus [GetActiveButton " &
        $index & "];break}")
  if updateAssignCrewCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv) != tclOk:
    return tclError
  let infoLabel = crewFrame & ".infolabel"
  tclEval(script = "ttk::label " & infoLabel & " -text {Available: " & $(
      module.owner.len - assigned) & "}")
  tclEval(script = "pack " & infoLabel)
  height = try:
      height + tclEval2(script = "winfo reqheight " & infoLabel).parseInt
    except:
      tclEval(script = "bgerror {Can't set the height2. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  try:
    if tclEval2(script = "winfo reqwidth " & infoLabel).parseInt > width:
      width = tclEval2(script = "winfo reqwidth " & infoLabel).parseInt
  except:
    tclEval(script = "bgerror {Can't set the width2. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  if height > 500:
    height = 500
  tclEval(script = crewCanvas & " create window 0 0 -anchor nw -window " & crewFrame)
  tclEval(script = "update")
  tclEval(script = crewCanvas & " configure -scrollregion [list " & tclEval2(
      script = crewCanvas & " bbox all") & "] -height " & $height & " -width " & $width)
  tclEval(script = "bind " & closeButton & " <Escape> {" & closeButton & " invoke;break}")
  tclEval(script = "bind " & closeButton & " <Tab> {focus [GetActiveButton 0];break}")
  showDialog(dialog = moduleDialog, relativeY = 0.2)
  return tclOk

proc showAssignSkillCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  let
    moduleIndex = try:
        ($argv[1]).parseInt - 1
      except:
        tclEval(script = "bgerror {Can't get module index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    moduleDialog = createDialog(name = ".moduledialog",
        title = "Assign skill to " & playerShip.modules[moduleIndex].name,
        titleWidth = 400)
    skillsFrame = moduleDialog & ".frame"
  tclEval(script = "ttk::frame " & skillsFrame)
  var skillsTable = createTable(parent = skillsFrame, headers = @["Skill",
      "Training tool"])
  for index, skill in skillsList:
    var
      protoIndex = -1
      toolName = ""
    if skill.tool.len > 0:
      protoIndex = findProtoItem(itemType = skill.tool)
      toolName = try:
          (if itemsList[protoIndex].showType.len > 0: itemsList[
              protoIndex].showType else: itemsList[protoIndex].itemType)
        except:
          tclEval(script = "bgerror {Can't get the tool name. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
    var
      skillName = skill.name
      toolColor = "green"
    try:
      if getItemAmount(itemType = itemsList[protoIndex].itemType) == 0:
        skillName.add(y = " (no tool)")
        toolColor = "red"
    except:
      tclEval(script = "bgerror {Can't check item amount. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
    addButton(table = skillsTable, text = skillName, tooltip = "Press mouse " &
        (if gameSettings.rightButton: "right" else: "left") &
        " button to set as trained skill", command = "AssignModule skill " &
        $argv[1] & " " & $index, column = 1)
    addButton(table = skillsTable, text = toolName, tooltip = "Press mouse " & (
        if gameSettings.rightButton: "right" else: "left") &
        " button to set as trained skill", command = "AssignModule skill " &
        $argv[1] & " " & $index, column = 2, newRow = true,
        color = toolColor)
  updateTable(table = skillsTable)
  tclEval(script = "grid " & skillsFrame & " -padx 2")
  tclEval(script = "update")
  tclEval(script = skillsTable.canvas & " configure -scrollregion [list " &
      tclEval2(script = skillsTable.canvas & " bbox all") & "]")
  tclEval(script = skillsTable.canvas & " xview moveto 0.0")
  tclEval(script = skillsTable.canvas & " yview moveto 0.0")
  addCloseButton(name = moduleDialog & ".button", text = "Close",
      command = "CloseDialog " & moduleDialog, row = 2)
  let dialogCloseButton = moduleDialog & ".button"
  tclEval(script = "bind " & dialogCloseButton & " <Tab> {focus " &
      skillsTable.canvas & ";break}")
  tclEval(script = "bind " & skillsTable.canvas & " <Escape> {" &
      dialogCloseButton & "invoke;break}")
  showDialog(dialog = moduleDialog, relativeY = 0.2)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the wait menu
  try:
    addCommand("ShowModuleInfo", showModuleInfoCommand)
    addCommand("SetUpgrade", setUpgradeCommand)
    addCommand("AssignModule", assignModuleCommand)
    addCommand("DisableEngine", disableEngineCommand)
    addCommand("StopUpgrading", stopUpgradingCommand)
    addCommand("SetRepair", setRepairCommand)
    addCommand("ResetDestination", resetDestinationCommand)
    addCommand("UpdateAssignCrew", updateAssignCrewCommand)
    addCommand("ShowAssignCrew", showAssignCrewCommand)
    addCommand("ShowAssignSkill", showAssignSkillCommand)
  except:
    tclEval(script = "bgerror {Can't add a Tcl command. Reason: " &
        getCurrentExceptionMsg() & "}")

import shipsui

proc setUpgradeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  try:
    startUpgrading(moduleIndex = ($argv[2]).parseInt() - 1, upgradeType = (
        $argv[1]).parseInt)
  except:
    tclEval(script = "bgerror {Can't set upgrade for the module. Reason: " &
        getCurrentExceptionMsg() & "}")
  try:
    updateOrders(ship = playerShip)
  except:
    tclEval(script = "brerror {Can't update crew orders. Reason: " &
        getCurrentExceptionMsg() & "}")
  updateMessages()
  updateHeader()
  return showShipInfoCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)

proc assignModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let
    moduleIndex = try:
        ($argv[2]).parseInt - 1
      except:
        tclEval(script = "bgerror {Can't get the module index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    assignIndex = try:
        ($argv[3]).parseInt
      except:
        tclEval(script = "bgerror {Can't get the assing index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  if argv[1] == "crew":

    proc updateOrder(order: CrewOrders) {.sideEffect, raises: [KeyError,
        CrewOrderError, CrewNoSpaceError, Exception], tags: [RootEffect].} =
      giveOrders(ship = playerShip, memberIndex = assignIndex,
          givenOrder = order, moduleIndex = moduleIndex)
      if playerShip.crew[assignIndex].order != order:
        tclSetVar(varName = ".moduledialog.canvas.frame.crewbutton" & $(
            assignIndex), newValue = "0")

    try:
      case modulesList[playerShip.modules[moduleIndex].protoIndex].mType
      of cabin:
        block modulesLoop:
          for module in playerShip.modules.mitems:
            if module.mType == ModuleType2.cabin:
              for owner in module.owner.mitems:
                if owner == assignIndex:
                  owner = -1
                  break modulesLoop
        var assigned = false
        for owner in playerShip.modules[moduleIndex].owner.mitems:
          if owner == -1:
            owner = assignIndex
            assigned = true
            break
        if not assigned:
          playerShip.modules[moduleIndex].owner[0] = assignIndex
        addMessage(message = "You assigned " & playerShip.modules[
            moduleIndex].name & " to " & playerShip.crew[assignIndex].name &
                ".",
            mType = orderMessage)
      of gun, harpoonGun:
        updateOrder(order = gunner)
      of alchemyLab .. greenhouse:
        updateOrder(order = craft)
      of medicalRoom:
        updateOrder(order = heal)
      of trainingRoom:
        updateOrder(order = train)
      else:
        discard
    except CrewNoSpaceError, CrewOrderError:
      showMessage(text = getCurrentExceptionMsg(), title = "Can't assign crew")
      return tclOk
    except:
      tclEval(script = "bgerror {Can't assign crew member to the module. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  elif argv[1] == "ammo":
    if playerShip.modules[moduleIndex].mType == ModuleType2.gun:
      playerShip.modules[moduleIndex].ammoIndex = assignIndex
    else:
      playerShip.modules[moduleIndex].harpoonIndex = assignIndex
    try:
      addMessage(message = "You assigned " & itemsList[playerShip.cargo[
          assignIndex].protoIndex].name & " to " & playerShip.modules[
          moduleIndex].name & ".", mType = orderMessage)
    except:
      tclEval(script = "bgerror {Can't show message about assigned ammo. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  elif argv[1] == "skill":
    if playerShip.modules[moduleIndex].trainedSkill == assignIndex:
      return tclOk
    playerShip.modules[moduleIndex].trainedSkill = assignIndex
    try:
      addMessage(message = "You prepared " & playerShip.modules[
          moduleIndex].name & " for training " & skillsList[assignIndex].name &
          ".", mType = orderMessage)
    except:
      tclEval(script = "bgerror {Can't show message about assigned skill. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
    updateMessages()
    return tclOk
  updateMessages()
  return showShipInfoCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)

proc disableEngineCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let moduleIndex = try:
      ($argv[1]).parseInt - 1
    except:
      tclEval(script = "bgerror {Can't set module index. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  if playerShip.modules[moduleIndex].disabled:
    playerShip.modules[moduleIndex].disabled = false
    addMessage(message = "You enabled " & playerShip.modules[moduleIndex].name &
        ".", mType = orderMessage)
  else:
    var canDisable = false
    for index, module in playerShip.modules:
      if module.mType == ModuleType2.engine and (not module.disabled and
          index != moduleIndex):
        canDisable = true
        break
    if not canDisable:
      showMessage(text = "You can't disable this engine because it is your last working engine.",
          title = "Can't disable engine")
      return tclOk
    playerShip.modules[moduleIndex].disabled = true
    addMessage(message = "You disabled " & playerShip.modules[
        moduleIndex].name & ".", mType = orderMessage)
  updateMessages()
  return showShipInfoCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)

proc stopUpgradingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  playerShip.upgradeModule = -1
  for index, member in playerShip.crew:
    if member.order == upgrading:
      try:
        giveOrders(ship = playerShip, memberIndex = index, givenOrder = rest)
      except CrewOrderError:
        showMessage(text = getCurrentExceptionMsg(),
            title = "Can't give orders")
        return tclOk
      except:
        tclEval(script = "bgerror {Can't give orders to a crew member. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
      break
  addMessage(message = "You stopped current upgrade.", mType = orderMessage)
  updateMessages()
  updateHeader()
  return showShipInfoCommand(clientData = clientData, interp = interp,
      argc = 2, argv = argv)

proc setRepairCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  if argv[1] == "assign":
    playerShip.repairModule = try:
        ($argv[2]).parseInt
      except:
        tclEval(script = "bgerror {Can't set the repair priority. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    addMessage(message = "You assigned " & playerShip.modules[
        playerShip.repairModule].name & " as repair priority.",
        mType = orderMessage)
  else:
    playerShip.repairModule = -1
    addMessage(message = "You removed repair priority.", mType = orderMessage)
  updateMessages()
  return showShipInfoCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)

proc resetDestinationCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  playerShip.destinationX = 0
  playerShip.destinationY = 0
  return showShipInfoCommand(clientData = clientData, interp = interp,
      argc = 2, argv = argv)

# Temporary code for interfacing with Ada

proc addAdaModulesCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
