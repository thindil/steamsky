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

import ../[game, config, types]
import coreui, table

var
  modulesTable: TableWidget
  modulesIndexes: seq[Natural]

proc getModuleInfo(moduleIndex: Natural): string =
  let module = playerShip.modules[moduleIndex]
  case module.mType
  of gun:
    if playerShip.modules[moduleIndex].ammoIndex in 0 ..
        playerShip.cargo.high and itemsList[playerShip.cargo[playerShip.modules[
        moduleIndex].ammoIndex].protoIndex].itemType == modulesList[
        playerShip.modules[moduleIndex].protoIndex].value:
      result = "Uses " & itemsList[playerShip.cargo[playerShip.modules[
          moduleIndex].ammoIndex].protoIndex].name & ", "
    else:
      result = "No ammunition assigned, "
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
