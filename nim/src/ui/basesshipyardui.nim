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
import coreui, mapsui, table, utilsui2

var
  installTable, removeTable: TableWidget
  installIndexes, removeIndexes: seq[Natural]

proc showShipyardCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Show the selected base shipyard
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowShipyard ?moduletype? ?modulename?
  ## Show the base shipyard and load all available and installed modules
  ## lists. Moduletype is the type of modules to show in available modules,
  ## modulename is the name of the module to search in available modules.
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
      maxSize = try:
          modulesList[module.protoIndex].value
        except:
          return showError(message = "Can't get max size.")
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
    page = try:
        (if argc == 4: ($argv[3]).parseInt else: 1)
      except:
        return showError(message = "Can't get page.")
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  var currentRow = 1
  for index in installIndexes:
    try:
      if modulesList[index].price == 0 or skyBases[baseIndex].reputation.level <
          modulesList[index].reputation:
        continue
    except:
      return showError(message = "Can't get proto module price.")
    if argc > 1:
      let moduleType = try:
          ($argv[1]).parseInt
        except:
          return showError(message = "Can't get module type.")
      try:
        if moduleType > 0 and moduleType != modulesList[index].mType.ord:
          continue
      except:
        return showError(message = "Can't get proto module's type.")
    try:
      if argc > 2 and argv[2].len > 0 and not modulesList[
          index].name.toLowerAscii.contains(sub = ($argv[2]).toLowerAscii):
        continue
    except:
      return showError(message = "Can't check modules' name.")
    if currentRow < startRow:
      currentRow.inc
      continue
    let moduleSize = try:
        (if modulesList[index].mType ==
          ModuleType.hull: modulesList[index].maxValue else: modulesList[index].size)
      except:
        return showError(message = "Can't get size of the module.")
    try:
      addButton(table = installTable, text = modulesList[index].name,
          tooltip = "Show the module's info", command = "ShowInstallInfo {" &
          $index & "}", column = 1)
    except:
      return showError(message = "Can't add button with name.")
    try:
      addButton(table = installTable, text = getModuleType(moduleIndex = index),
          tooltip = "Show the module's info", command = "ShowInstallInfo {" &
          $index & "}", column = 2)
    except:
      return showError(message = "Can't add button with type.")
    try:
      addButton(table = installTable, text = $moduleSize,
          tooltip = "Show the module's info", command = "ShowInstallInfo {" &
          $index & "}", column = 3, newRow = false, color = (if modulesList[
              index].mType == ModuleType.hull: (if moduleSize <
              allSpace: "red" elif moduleSize >
              allSpace: "green" else: "") else: (if moduleSize >
              maxSize: "red" else: "")))
    except:
      return showError(message = "Can't add button with size.")
    try:
      addButton(table = installTable, text = modulesList[index].repairMaterial,
          tooltip = "Show the module's info", command = "ShowInstallInfo {" &
          $index & "}", column = 4)
    except:
      return showError(message = "Can't add button with repair material.")
    var cost = try:
        modulesList[index].price
      except:
        return showError(message = "Can't get cost.")
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk))
    except:
      return showError(message = "Can't count price.")
    addButton(table = installTable, text = $cost,
        tooltip = "Show the module's info", command = "ShowInstallInfo {" &
        $index & "}", column = 5, newRow = true, color = (if moneyIndex2 >
            -1 and cost <= playerShip.cargo[
            moneyIndex2].amount: "" else: "red"))
    if installTable.row == gameSettings.listsLimit + 1:
      break
  addPagination(table = installTable, previousCommand = (if page >
      1: "ShowShipyard " & arguments & " " & $(page - 1) else: ""),
      nextCommand = (if installTable.row < gameSettings.listsLimit +
      1: "" else: "ShowShipyard " & arguments & " " & $(page + 1)))
  updateTable(table = installTable, grabFocus = tclEval2(script = "focus") != searchEntry)
  if removeIndexes.len != playerShip.modules.len:
    removeIndexes = @[]
    for index, _ in playerShip.modules:
      removeIndexes.add(y = index)
  clearTable(table = removeTable)
  currentRow = 1
  for index in removeIndexes:
    try:
      if modulesList[playerShip.modules[index].protoIndex].mType ==
          ModuleType.hull:
        continue
    except:
      return showError(message = "Can't check module type.")
    if currentRow < startRow:
      currentRow.inc
      continue
    addButton(table = removeTable, text = playerShip.modules[index].name,
        tooltip = "Show the module's info", command = "ShowRemoveInfo {" &
        $(index + 1) & "}", column = 1)
    try:
      addButton(table = removeTable, text = getModuleType(
          moduleIndex = playerShip.modules[index].protoIndex),
              tooltip = "Show the module's info", command = "ShowRemoveInfo {" &
          $(index + 1) & "}", column = 2)
    except:
      return showError(message = "Can't add button with player's ship module type.")
    try:
      addButton(table = removeTable, text = $modulesList[playerShip.modules[
          index].protoIndex].size, tooltip = "Show the module's info",
              command = "ShowRemoveInfo {" &
          $(index + 1) & "}", column = 3)
    except:
      return showError(message = "Can't add button with player's ship module size.")
    try:
      addButton(table = removeTable, text = $modulesList[playerShip.modules[
          index].protoIndex].repairMaterial, tooltip = "Show the module's info",
              command = "ShowRemoveInfo {" &
          $(index + 1) & "}", column = 4)
    except:
      return showError(message = "Can't add button with player's ship repair material.")
    let damage = 1.0 - (playerShip.modules[index].durability.float /
        playerShip.modules[index].maxDurability.float)
    var cost: Natural = try:
        modulesList[playerShip.modules[
          index].protoIndex].price - (modulesList[playerShip.modules[
          index].protoIndex].price.float * damage).int
      except:
        return showError(message = "Can't get cost of player's ship module.")
    if cost == 0:
      cost = 1
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk),
          reduce = false)
    except:
      return showError(message = "Can't count cost of player's ship module.")
    addButton(table = removeTable, text = $cost,
        tooltip = "Show the module's info", command = "ShowRemoveInfo {" &
        $(index + 1) & "}", column = 5, newRow = true)
    if removeTable.row == gameSettings.listsLimit + 1:
      break
  addPagination(table = removeTable, previousCommand = (if page >
      1: "ShowShipyard " & arguments & " " & $(page - 1) else: ""),
      nextCommand = (if installTable.row < gameSettings.listsLimit +
      1: "" else: "ShowShipyard " & arguments & " " & $(page + 1)))
  updateTable(table = removeTable)
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  tclEval(script = shipyardCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = shipyardCanvas & " xview moveto 0.0")
  tclEval(script = shipyardCanvas & " yview moveto 0.0")
  showScreen(newScreenName = "shipyardframe")
  tclSetResult(value = "1")
  tclEval(script = "ShowShipyardTab show")
  return tclOk

var moduleIndex: Natural = 0

proc setModuleInfo(installing: bool; row: var Positive; newInfo: bool = true) =
  row.inc
  var
    mType: ModuleType
    maxValue, value, weight, maxOwners, shipModuleIndex, cost: Natural = 0
    size: Positive = 1
    speed, moneyIndex2: int = -1
    moduleLabel = ""
  if installing:
    mType = modulesList[moduleIndex].mType
    maxValue = modulesList[moduleIndex].maxValue
    value = modulesList[moduleIndex].value
    size = modulesList[moduleIndex].size
    weight = modulesList[moduleIndex].weight
    maxOwners = modulesList[moduleIndex].maxOwners
    speed = modulesList[moduleIndex].speed
    moduleLabel = ".moduledialog.cost"
    let compareBox = ".moduledialog.compare.combo"
    var moduleIterator = 1
    if tclEval2(script = "winfo ismapped " & compareBox) == "1":
      moduleIterator = tclEval2(script = compareBox & " current").parseInt + 1
    for index, module in playerShip.modules:
      if modulesList[module.protoIndex].mType == mType:
        moduleIterator.dec
        if moduleIterator == 0:
          shipModuleIndex = index
          break
    cost = modulesList[moduleIndex].price
    countPrice(price = cost, traderIndex = findMember(order = talk))
    moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    tclEval(script = moduleLabel & " configure -text {" & $cost & " " &
        moneyName & "} " & (if moneyIndex == -1 or playerShip.cargo[
        moneyIndex2].amount <
        cost: "-style Headerred.TLabel" else: "-style Golden.TLabel"))
    moduleLabel = ".moduledialog.time"
    tclEval(script = moduleLabel & " -text {" & $modulesList[
        moduleIndex].installTime & " minutes} -style Golden.TLabel")
  else:
    shipModuleIndex = moduleIndex
    mType = modulesList[playerShip.modules[shipModuleIndex].protoIndex].mType
    case mType
    of harpoonGun:
      maxValue = playerShip.modules[shipModuleIndex].duration
      value = modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
    of engine:
      maxValue = playerShip.modules[shipModuleIndex].power
      value = playerShip.modules[shipModuleIndex].fuelUsage
    of cabin:
      maxValue = playerShip.modules[shipModuleIndex].quality
      value = playerShip.modules[shipModuleIndex].cleanliness
    of gun:
      maxValue = playerShip.modules[shipModuleIndex].damage
      value = modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
    of cargo:
      maxValue = modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxValue
      value = modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
    of hull:
      maxValue = playerShip.modules[shipModuleIndex].maxModules
      value = modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
    of batteringRam:
      maxValue = playerShip.modules[shipModuleIndex].damage2
      value = 0
    else:
      maxValue = 0
      value = 0
    size = modulesList[playerShip.modules[shipModuleIndex].protoIndex].size
    weight = modulesList[playerShip.modules[shipModuleIndex].protoIndex].weight
    maxOwners = modulesList[playerShip.modules[
        shipModuleIndex].protoIndex].maxOwners
    speed = modulesList[playerShip.modules[shipModuleIndex].protoIndex].speed
  case mType
  of hull:
    if installing:
      if newInfo:
        moduleLabel = ".moduledialog.hullinfo"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Ship hull can be only replaced.} -style Golden.TLabel")
        row.inc
        tclEval(script = "grid " & moduleLabel & " -sticky w -columnspan 2 -padx {5 0}")
        moduleLabel = ".moduledialog.moduleslbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Modules space:}")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        moduleLabel = ".moduledialog.modules"
        tclEval(script = "ttk::label " & moduleLabel)
      else:
        moduleLabel = ".moduledialog.modules"
      if maxValue < playerShip.modules[shipModuleIndex].maxModules:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (smaller)} -style Headerred.TLabel")
      elif maxValue > playerShip.modules[shipModuleIndex].maxModules:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (bigger)} -style Headergreen.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
      if newInfo:
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
        row.inc
        moduleLabel = ".moduledialog.maxsizelbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Max module size:}")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        tclEval(script = "ttk::label " & moduleLabel)
      else:
        moduleLabel = ".moduledialog.maxsize"
      if value < modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].value:
        tclEval(script = moduleLabel & " configure -text {" & $value & " (smaller)} -style Headerred.TLabel")
      elif value > modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].value:
        tclEval(script = moduleLabel & " configure -text {" & $value & " (bigger)} -style Headergreen.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {" & $value & "} -style Golden.TLabel")
      if newInfo:
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
        row.inc
  of engine:
    if newInfo:
      moduleLabel = ".moduledialog.powerlbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Max power:}")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.power"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.power"
    if installing and shipModuleIndex > -1:
      if maxValue < playerShip.modules[shipModuleIndex].power:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (weaker)} -style Headerred.TLabel")
      elif maxValue > playerShip.modules[shipModuleIndex].power:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (stronger)} -style Headergreen.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
      if newInfo:
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
        row.inc
        moduleLabel = ".moduledialog.fuellbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Fuel usage:}")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        moduleLabel = ".moduledialog.fuel"
        tclEval(script = "ttk::label " & moduleLabel)
      else:
        moduleLabel = ".moduledialog.fuel"
      if value < playerShip.modules[shipModuleIndex].fuelUsage:
        tclEval(script = moduleLabel & " configure -text {" & $value & " (less)} -style Headergreen.TLabel")
      elif value > playerShip.modules[shipModuleIndex].fuelUsage:
        tclEval(script = moduleLabel & " configure -text {" & $value & " (more)} -style Headerred.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {" & $value & "} -style Golden.TLabel")
      if newInfo:
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
  else:
    discard

# Temporary code for testing
var tmp: Positive = 1
setModuleInfo(installing = true, row = tmp)

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowShipyard", showShipyardCommand)
  except:
    showError(message = "Can't add a Tcl command.")
