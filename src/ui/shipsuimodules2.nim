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

## Provides various code related to showing the player's ship module's
## information, like updating module's info, etc. Split from the
## shipmodulesui module to avoid circular dependencies.

import std/tables
import contracts
import ../[game, config, crafts, tk, types]
import coreui, table

var
  modulesTable*: TableWidget
    ## The UI table with all the installed the player's ship's modules
  modulesIndexes*: seq[Natural]
    ## The list of indexes of the installed modules

proc getModuleInfo*(moduleIndex: Natural): string {.raises: [],
    tags: [], contractual.} =
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
      result.add(y = " no gunner.")
    else:
      result.add(y = " " & playerShip.crew[module.owner[0]].name & " is gunner.")
  of workshop:
    let recipeName = try:
        getWorkshopRecipeName(workshop = moduleIndex)
      except:
        ""
    if recipeName.len > 0:
      result = recipeName
      var hasWorkers = false
      for owner in module.owner:
        if owner > -1:
          if hasWorkers:
            result.add(y = ", " & playerShip.crew[owner].name)
          else:
            result.add(y = ", workers: " & playerShip.crew[owner].name)
          hasWorkers = true
      if not hasWorkers:
        result.add(y = ", no workers assigned")
      result.add(y = ".")
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
              result.add(y = ", " & playerShip.crew[owner].name)
            else:
              result.add(y = ", trainees: " & playerShip.crew[owner].name)
            hasTrainees = true
        if not hasTrainees:
          result.add(y = ", no trainees assigned")
        result.add(y = ".")
      else:
        result = "Not set for training."
    except:
      result = "Not set for training."
  else:
    discard

proc updateModulesInfo*(page: Positive = 1) {.raises: [],
    tags: [RootEffect], contractual.} =
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
      modulesIndexes.add(y = index)
  clearTable(table = modulesTable)
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
