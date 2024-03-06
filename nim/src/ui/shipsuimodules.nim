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
import ../[game, config, crafts, tk, types]
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
  ## Show the module's damage
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
  if playerShip.repairModule == moduleIndex:
    infoButton = moduleFrame & ".repairbutton"
    tclEval(script = "ttk::button " & infoButton &
        " -image cancelicon -command {" & closeDialogButton & " invoke;SetRepair remove} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Remove the repair priority\"")
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
