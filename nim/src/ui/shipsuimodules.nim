# Copyright 2023 Bartek thindil Jasicki
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

import std/tables
import ../[game, config, crafts, types]
import coreui, table

var
  modulesTable: TableWidget
  modulesIndexes: seq[Natural]

proc getModuleInfo(moduleIndex: Natural): string =
  let module = playerShip.modules[moduleIndex]
  case module.mType
  of gun:
    if module.ammoIndex in 0 ..
        playerShip.cargo.high and itemsList[playerShip.cargo[
            module.ammoIndex].protoIndex].itemType == itemsTypesList[
            modulesList[module.protoIndex].value]:
      result = "Uses " & itemsList[playerShip.cargo[
          module.ammoIndex].protoIndex].name & ", "
    else:
      result = "No ammunition assigned, "
    if module.owner[0] == -1:
      result.add(" no gunner.")
    else:
      result.add(" " & playerShip.crew[module.owner[0]].name & " is gunner.")
  of workshop:
    let recipeName = getWorkshopRecipeName(moduleIndex)
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
  else:
    discard

proc updateModulesInfo*(page: Positive = 1) =
  let
    shipCanvas = mainPaned & ".shipinfoframe.modules.canvas"
    shipInfoFrame = shipCanvas & ".frame"
  if modulesTable.rowHeight == 1:
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
  var currentRow = 1
  for index in modulesIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    addButton(table = modulesTable, text = playerShip.modules[index].name,
        tooltip = "Show the module's info", command = "ShowModuleInfo " &
        $index, column = 1)
    addProgressbar(table = modulesTable, value = playerShip.modules[
        index].durability, maxValue = playerShip.modules[index].maxDurability,
        tooltip = "Show the module's info", command = "ShowModuleInfo " &
        $index, column = 2)
    addButton(table = modulesTable, text = getModuleInfo(moduleIndex = index),
        tooltip = "Show the module's info", command = "ShowModuleInfo " &
        $index, column = 3, newRow = true)
