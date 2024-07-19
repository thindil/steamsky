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

import std/[os, strutils, tables]
import ../[bases, config, crewinventory, game, maps, shipscrew, shipmodules, tk, types]
import coreui, mapsui, table

var
  installTable, removeTable: TableWidget
  installIndexes, removeIndexes: seq[Natural]

proc showShipyardCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  var shipyardFrame = mainPaned & ".shipyardframe"
  let
    shipyardCanvas = shipyardFrame & ".canvas"
    moduleTypeBox = shipyardCanvas & ".shipyard.install.options.modules"
  if tclEval2(script = "winfo exists " & shipyardCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "shipyard.tcl")
    tclEval(script = "bind " & shipyardFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    shipyardFrame = shipyardCanvas & ".shipyard.install"
    installTable = createTable(parent = shipyardFrame, headers = @["Name",
        "Type", "Size", "Materials", "Cost"],
        scrollbar = ".gameframe.paned.shipyardframe.scrolly", command = "",
        tooltipText = "Press mouse button to sort the modules.")
    shipyardFrame = shipyardCanvas & ".shipyard.remove"
    removeTable = createTable(parent = shipyardFrame, headers = @["Name",
        "Type", "Size", "Materials", "Price"],
        scrollbar = ".gameframe.paned.shipyardframe.scrolly",
        command = "SortShipyardModules remove 0 {}",
        tooltipText = "Press mouse button to sort the modules.")
  elif tclEval2(script = "winfo ismapped " & shipyardCanvas) == "1":
    if argc == 1:
      tclEval(script = "grid remove " & closeButton)
      showSkyMap(clear = true)
      return tclOk
    tclEval(script = moduleTypeBox & " current " & $argv[1])
  elif tclEval2(script = "winfo ismapped " & shipyardCanvas) == "0" and argc == 1:
    tclEval(script = moduleTypeBox & " current 0")
  tclSetVar(varName = "gamestate", newValue = "repair")
  var
    maxSize, allSpace = 1
    usedSpace = 0
  for module in playerShip.modules:
    if module.mType == ModuleType2.hull:
      maxSize = modulesList[module.protoIndex].value
      usedSpace = module.installedModules
      allSpace = module.maxModules
      break
  shipyardFrame = shipyardCanvas & ".shipyard"
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  var installInfo = (if moneyIndex2 > -1: "You have " & $playerShip.cargo[
      moneyIndex2].amount & " " & moneyName &
      "." else: "\nYou don't have any " & moneyName & " to install anything.")
  installInfo.add(y = "\nYou have used " & $usedSpace &
      " modules space from max " & $allSpace & " allowed.")
  let moneyLabel = shipyardCanvas & ".shipyard.moneyinfo"
  tclEval(script = moneyLabel & " configure -text {" & installInfo & "}")
  tclEval(script = "SetScrollbarBindings " & moneyLabel & " .gameframe.paned.shipyardframe.scrolly")
  let searchEntry = shipyardCanvas & ".shipyard.install.options.search"
  if argc < 3:
    tclEval(script = searchEntry & " configure -validatecommand {}")
    tclEval(script = searchEntry & " delete 0 end")
    tclEval(script = searchEntry & " -validatecommand {ShowShipyard [" &
        shipyardFrame & ".install.options.modules current] %P}")
  if installIndexes.len == 0:
    for index in modulesList.keys:
      installIndexes.add(y = index)
  let arguments = (if argc > 2: "{" & $argv[1] & "} {" & $argv[2] &
      "}" elif argc == 2: "{" & $argv[1] & "} {}" else: "0 {}")
  updateHeadersCommand(table = installTable,
      command = "SortShipyardModules install " & arguments)
  clearTable(table = installTable)
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    page = (if argc == 4: ($argv[3]).parseInt else: 1)
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  var currentRow = 1
  for index in installIndexes:
    if modulesList[index].price == 0 or skyBases[baseIndex].reputation.level <
        modulesList[index].reputation:
      continue
    let moduleType = ($argv[1]).parseInt
    if argc > 1 and moduleType > 0 and moduleType != modulesList[
        index].mType.ord:
      continue
    if argc > 2 and argv[2].len > 0 and not modulesList[
        index].name.toLowerAscii.contains(sub = ($argv[2]).toLowerAscii):
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let moduleSize = (if modulesList[index].mType ==
        ModuleType.hull: modulesList[index].maxValue else: modulesList[index].size)
    addButton(table = installTable, text = modulesList[index].name,
        tooltip = "Show the module's info", command = "ShowInstallInfo {" &
        $index & "}", column = 1)
    addButton(table = installTable, text = getModuleType(moduleIndex = index),
        tooltip = "Show the module's info", command = "ShowInstallInfo {" &
        $index & "}", column = 2)
    addButton(table = installTable, text = $moduleSize,
        tooltip = "Show the module's info", command = "ShowInstallInfo {" &
        $index & "}", column = 3, newRow = false, color = (if modulesList[
            index].mType == ModuleType.hull: (if moduleSize <
            allSpace: "red" elif moduleSize >
            allSpace: "green" else: "") else: (if moduleSize >
            maxSize: "red" else: "")))
    addButton(table = installTable, text = modulesList[index].repairMaterial,
        tooltip = "Show the module's info", command = "ShowInstallInfo {" &
        $index & "}", column = 4)
    var cost = modulesList[index].price
    countPrice(price = cost, traderIndex = findMember(order = talk))
    addButton(table = installTable, text = $cost,
        tooltip = "Show the module's info", command = "ShowInstallInfo {" &
        $index & "}", column = 5, newRow = true, color = (if moneyIndex2 >
            -1 and cost <= playerShip.cargo[
            moneyIndex2].amount: "" else: "red"))
    if installTable.row == gameSettings.listsLimit + 1:
      break
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowShipyard", showShipyardCommand)
  except:
    showError(message = "Can't add a Tcl command.")
