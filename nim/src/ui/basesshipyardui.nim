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
import ../[bases, basesship2, config, crewinventory, game, maps, shipscrew,
    shipmodules, tk, types]
import coreui, dialogs, mapsui, table, utilsui2

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
      nextCommand = (if removeTable.row < gameSettings.listsLimit +
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

proc setModuleInfo(installing: bool; row: var Positive;
    newInfo: bool = true) {.sideEffect, raises: [], tags: [].} =
  ## Show information about selected module
  ##
  ## installing - If true, player looking at installing modules list
  ## row        - The current row in the dialog
  ## newInfo    - If true, create the new UI for the info, otherwise reuse old
  ##              one. Default value is True.
  row.inc
  var
    mType: ModuleType
    maxValue, value, weight, maxOwners, cost: Natural = 0
    size: Positive = 1
    speed, moneyIndex2, shipModuleIndex: int = -1
    moduleLabel = ""
  if installing:
    mType = try:
        modulesList[moduleIndex].mType
      except:
        showError(message = "Can't get protomodule type")
        return
    maxValue = try:
        modulesList[moduleIndex].maxValue
      except:
        showError(message = "Can't get protomodule max value")
        return
    value = try:
        modulesList[moduleIndex].value
      except:
        showError(message = "Can't get protomodule value")
        return
    size = try:
        modulesList[moduleIndex].size
      except:
        showError(message = "Can't get protomodule size")
        return
    weight = try:
        modulesList[moduleIndex].weight
      except:
        showError(message = "Can't get protomodule weight")
        return
    maxOwners = try:
        modulesList[moduleIndex].maxOwners
      except:
        showError(message = "Can't get protomodule max owners")
        return
    speed = try:
        modulesList[moduleIndex].speed
      except:
        showError(message = "Can't get protomodule speed")
        return
    moduleLabel = ".moduledialog.cost"
    let compareBox = ".moduledialog.compare.combo"
    var moduleIterator = 1
    if tclEval2(script = "winfo ismapped " & compareBox) == "1":
      moduleIterator = try:
          tclEval2(script = compareBox & " current").parseInt + 1
        except:
          showError(message = "Can't get protomodule iterator")
          return
    for index, module in playerShip.modules:
      try:
        if modulesList[module.protoIndex].mType == mType:
          moduleIterator.dec
          if moduleIterator == 0:
            shipModuleIndex = index
            break
      except:
        showError(message = "Can't get ship module index")
        return
    cost = try:
        modulesList[moduleIndex].price
      except:
        showError(message = "Can't get protomodule cost")
        return
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk))
    except:
      showError(message = "Can't count protomodule cost")
      return
    moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    tclEval(script = moduleLabel & " configure -text {" & $cost & " " &
        moneyName & "} " & (if moneyIndex == -1 or playerShip.cargo[
        moneyIndex2].amount <
        cost: "-style Headerred.TLabel" else: "-style Golden.TLabel"))
    moduleLabel = ".moduledialog.time"
    try:
      discard tclEval(script = moduleLabel & " configure -text {" &
          $modulesList[moduleIndex].installTime & " minutes} -style Golden.TLabel")
    except:
      showError(message = "Can't show install time")
      return
  else:
    shipModuleIndex = moduleIndex - 1
    mType = try:
        modulesList[playerShip.modules[shipModuleIndex].protoIndex].mType
      except:
        showError(message = "Can't get module type")
        return
    case mType
    of harpoonGun:
      maxValue = playerShip.modules[shipModuleIndex].duration
      value = try:
          modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
      except:
        showError(message = "Can't get module value")
        return
    of engine:
      maxValue = playerShip.modules[shipModuleIndex].power
      value = playerShip.modules[shipModuleIndex].fuelUsage
    of cabin:
      maxValue = playerShip.modules[shipModuleIndex].quality
      value = playerShip.modules[shipModuleIndex].cleanliness
    of gun:
      maxValue = playerShip.modules[shipModuleIndex].damage
      value = try:
          modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
        except:
          showError(message = "Can't get module value2")
          return
    of cargo:
      maxValue = try:
          modulesList[playerShip.modules[shipModuleIndex].protoIndex].maxValue
        except:
          showError(message = "Can't get module max value")
          return
      value = try:
          modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
        except:
          showError(message = "Can't get module value3")
          return
    of hull:
      maxValue = playerShip.modules[shipModuleIndex].maxModules
      value = try:
          modulesList[playerShip.modules[shipModuleIndex].protoIndex].value
        except:
          showError(message = "Can't get module value4")
          return
    of batteringRam:
      maxValue = playerShip.modules[shipModuleIndex].damage2
      value = 0
    else:
      maxValue = 0
      value = 0
    size = try:
        modulesList[playerShip.modules[shipModuleIndex].protoIndex].size
      except:
        showError(message = "Can't get module size")
        return
    weight = try:
        modulesList[playerShip.modules[shipModuleIndex].protoIndex].weight
      except:
        showError(message = "Can't get module weight")
        return
    maxOwners = try:
        modulesList[playerShip.modules[shipModuleIndex].protoIndex].maxOwners
      except:
        showError(message = "Can't get module max owners")
        return
    speed = try:
        modulesList[playerShip.modules[shipModuleIndex].protoIndex].speed
      except:
        showError(message = "Can't get module size")
        return
  case mType
  of hull:
    if installing:
      if newInfo:
        moduleLabel = ".moduledialog.hullinfo"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Ship hull can be only replaced.} -style Golden.TLabel")
        row.inc
        tclEval(script = "grid " & moduleLabel & " -sticky w -columnspan 2 -padx {5 0}")
        moduleLabel = ".moduledialog.moduleslbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Modules space: }")
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
        tclEval(script = "ttk::label " & moduleLabel & " -text {Max module size: }")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        moduleLabel = ".moduledialog.maxsize"
        tclEval(script = "ttk::label " & moduleLabel)
      else:
        moduleLabel = ".moduledialog.maxsize"
      try:
        if value < modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].value:
          tclEval(script = moduleLabel & " configure -text {" & $value & " (smaller)} -style Headerred.TLabel")
        elif value > modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].value:
          tclEval(script = moduleLabel & " configure -text {" & $value & " (bigger)} -style Headergreen.TLabel")
        else:
          discard tclEval(script = moduleLabel & " configure -text {" & $value & "} -style Golden.TLabel")
      except:
        showError(message = "Can't show module size")
        return
      if newInfo:
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
        row.inc
  of engine:
    if newInfo:
      moduleLabel = ".moduledialog.powerlbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Max power: }")
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
        tclEval(script = "ttk::label " & moduleLabel & " -text {Fuel usage: }")
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
        row.inc
    else:
      if newInfo:
        row.dec
        moduleLabel = ".moduledialog.power"
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      else:
        moduleLabel = ".moduledialog.power"
      tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
      if newInfo:
        row.inc
        moduleLabel = ".moduledialog.fuellbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Fuel usage: }")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        moduleLabel = ".moduledialog.fuel"
        tclEval(script = "ttk::label " & moduleLabel)
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
        row.inc
      else:
        moduleLabel = ".moduledialog.fuel"
      tclEval(script = moduleLabel & " configure -text {" & $value & "} -style Golden.TLabel")
  of cargo:
    if newInfo:
      moduleLabel = ".moduledialog.cargolbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Max cargo: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.cargo"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.cargo"
    if installing and shipModuleIndex > -1:
      try:
        if maxValue > modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxValue:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " kg (bigger)} -style Headergreen.TLabel")
        elif maxValue < modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxValue:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " kg (smaller)} -style Headerred.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " kg} -style Golden.TLabel")
      except:
        showError(message = "Can't show module weight")
        return
    else:
      if newInfo:
        row.dec
      tclEval(script = moduleLabel & " configure -text {" & $maxValue & " kg} -style Golden.TLabel")
    if newInfo:
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      row.inc
  of cabin:
    if newInfo:
      moduleLabel = ".moduledialog.qualitylbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Quality: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.quality"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.quality"
    if installing and shipModuleIndex > -1:
      if maxValue < 30:
        if playerShip.modules[shipModuleIndex].quality > maxValue:
          tclEval(script = moduleLabel & " configure -text {minimal (worse)} -style Headerred.TLabel")
        elif playerShip.modules[shipModuleIndex].quality < maxValue:
          tclEval(script = moduleLabel & " configure -text {minimal (better)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {minimal} -style Golden.TLabel")
      elif maxValue < 60:
        if playerShip.modules[shipModuleIndex].quality > maxValue:
          tclEval(script = moduleLabel & " configure -text {basic (worse)} -style Headerred.TLabel")
        elif playerShip.modules[shipModuleIndex].quality < maxValue:
          tclEval(script = moduleLabel & " configure -text {basic (better)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {basic} -style Golden.TLabel")
      elif maxValue < 80:
        if playerShip.modules[shipModuleIndex].quality > maxValue:
          tclEval(script = moduleLabel & " configure -text {extended (worse)} -style Headerred.TLabel")
        elif playerShip.modules[shipModuleIndex].quality < maxValue:
          tclEval(script = moduleLabel & " configure -text {extended (better)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {extended} -style Golden.TLabel")
      else:
        if playerShip.modules[shipModuleIndex].quality > maxValue:
          tclEval(script = moduleLabel & " configure -text {luxury (worse)} -style Headerred.TLabel")
        elif playerShip.modules[shipModuleIndex].quality < maxValue:
          tclEval(script = moduleLabel & " configure -text {luxury (better)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {luxury} -style Golden.TLabel")
    else:
      row.dec
      if maxValue < 30:
        tclEval(script = moduleLabel & " configure -text {minimal} -style Golden.TLabel")
      elif maxValue < 60:
        tclEval(script = moduleLabel & " configure -text {basic} -style Golden.TLabel")
      elif maxValue < 80:
        tclEval(script = moduleLabel & " configure -text {extended} -style Golden.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {luxury} -style Golden.TLabel")
    if newInfo:
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      row.inc
      moduleLabel = ".moduledialog.ownerslbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Max owners: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.owners"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.owners"
    if installing and shipModuleIndex > -1:
      try:
        if modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxOwners > maxOwners:
          tclEval(script = moduleLabel & " configure -text {" & $maxOwners & " (less)} -style Headerred.TLabel")
        elif modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxOwners < maxOwners:
          tclEval(script = moduleLabel & " configure -text {" & $maxOwners & " (more)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {" & $maxOwners & "} -style Golden.TLabel")
      except:
        showError(message = "Can't show module owners")
        return
    else:
      tclEval(script = moduleLabel & " configure -text {" & $maxOwners & "} -style Golden.TLabel")
    if newInfo:
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
    row.inc
  of alchemyLab..greenhouse:
    if newInfo:
      moduleLabel = ".moduledialog.workerslbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Max workers:}")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      if installing and shipModuleIndex == -1:
        row.inc
      moduleLabel = ".moduledialog.workers"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.workers"
    if installing and shipModuleIndex > -1:
      try:
        if modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxOwners > maxOwners:
          tclEval(script = moduleLabel & " configure -text {" & $maxOwners & " (less)} -style Headerred.TLabel")
        elif modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxOwners < maxOwners:
          tclEval(script = moduleLabel & " configure -text {" & $maxOwners & " (more)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {" & $maxOwners & "} -style Golden.TLabel")
      except:
        showError(message = "Can't show module workers")
        return
    else:
      if newInfo:
        row.dec
      tclEval(script = moduleLabel & " configure -text {" & $maxOwners & "} -style Golden.TLabel")
    if newInfo:
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      row.inc
  of gun, harpoonGun:
    if newInfo:
      moduleLabel = ".moduledialog.strengthlbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Strength: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      if installing and shipModuleIndex == -1:
        row.inc
      moduleLabel = ".moduledialog.strength"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.strength"
    if installing and shipModuleIndex > -1:
      if mType == ModuleType.gun:
        if playerShip.modules[shipModuleIndex].damage > maxValue:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (weaker)} -style Headerred.TLabel")
        elif playerShip.modules[shipModuleIndex].damage < maxValue:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (stronger)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
      else:
        if playerShip.modules[shipModuleIndex].duration > maxValue:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (weaker)} -style Headerred.TLabel")
        elif playerShip.modules[shipModuleIndex].duration < maxValue:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (stronger)} -style Headergreen.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
    else:
      row.dec
      tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
    if newInfo:
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      row.inc
      moduleLabel = ".moduledialog.ammolbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Ammunition: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.ammo"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.ammo"
    for item in itemsList.values:
      if item.itemType == itemsTypesList[value - 1]:
        tclEval(script = moduleLabel & " configure -text {Any" & item.name[
            item.name.find(sub = ' ')..^1] & "} -style Golden.TLabel")
        if newInfo:
          tclEval(script = "grid " & moduleLabel &
              " -sticky w -column 1 -row " & $row)
          row.inc
        break
    if mType == ModuleType.gun:
      if newInfo:
        moduleLabel = ".moduledialog.ratelbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Max fire rate: }")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        moduleLabel = ".moduledialog.rate"
        tclEval(script = "ttk::label " & moduleLabel)
      else:
        moduleLabel = ".moduledialog.rate"
      if installing and shipModuleIndex > -1:
        try:
          if modulesList[playerShip.modules[shipModuleIndex].protoIndex].speed > speed:
            if speed > 0:
              tclEval(script = moduleLabel & " configure -text {" & $speed & "/round (slower)} -style Headerred.TLabel")
            else:
              tclEval(script = moduleLabel & " configure -text {" & $(
                  speed.abs) & " rounds (slower)} -style Headerred.TLabel")
          elif modulesList[playerShip.modules[
              shipModuleIndex].protoIndex].speed < speed:
            if speed > 0:
              tclEval(script = moduleLabel & " configure -text {" & $speed & "/round (faster)} -style Headergreen.TLabel")
            else:
              tclEval(script = moduleLabel & " configure -text {" & $(
                  speed.abs) & " rounds (faster)} -style Headergreen.TLabel")
          else:
            if speed > 0:
              tclEval(script = moduleLabel & " configure -text {" & $speed & "/round} -style Golden.TLabel")
            else:
              tclEval(script = moduleLabel & " configure -text {" & $(
                  speed.abs) & " rounds} -style Golden.TLabel")
        except:
          showError(message = "Can't show fire rate")
          return
      else:
        if speed > 0:
          tclEval(script = moduleLabel & " configure -text {" & $speed & "/round} -style Golden.TLabel")
        else:
          tclEval(script = moduleLabel & " configure -text {" & $(speed.abs) & " rounds} -style Golden.TLabel")
      if newInfo:
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
        row.inc
  of batteringRam:
    if newInfo:
      if installing and shipModuleIndex == -1:
        row.inc
      moduleLabel = ".moduledialog.strengthlbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Strength: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.strength"
      tclEval(script = "ttk::label " & moduleLabel)
    else:
      moduleLabel = ".moduledialog.strength"
    if installing and shipModuleIndex > -1:
      if playerShip.modules[shipModuleIndex].damage2 > maxValue:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (weaker)} -style Headerred.TLabel")
      elif playerShip.modules[shipModuleIndex].damage2 < maxValue:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & " (stronger)} -style Headergreen.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
    else:
      row.dec
      tclEval(script = moduleLabel & " configure -text {" & $maxValue & "} -style Golden.TLabel")
    if newInfo:
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      row.inc
  else:
    discard
  if mType in {ModuleType.armor, turret, cockpit} and not installing:
    row.dec
  if mType notin {ModuleType.hull, armor}:
    if newInfo:
      moduleLabel = ".moduledialog.sizelbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Size: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.size"
      tclEval(script = "ttk::label " & moduleLabel)
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
    else:
      moduleLabel = ".moduledialog.size"
    var added = false
    if installing:
      for module in playerShip.modules:
        try:
          if module.mType == ModuleType2.hull and size > modulesList[
              module.protoIndex].value:
            tclEval(script = moduleLabel & " configure -text {" & $size & " (need a bigger hull)} -style Headerred.TLabel")
            added = true
            break
        except:
          showError(message = "Can't show module's size")
          return
    if not added:
      tclEval(script = moduleLabel & " configure -text {" & $size & "} -style Golden.TLabel")
    row.inc
  if weight > 0:
    if shipModuleIndex > -1:
      if newInfo:
        moduleLabel = ".moduledialog.weightlbl"
        tclEval(script = "ttk::label " & moduleLabel & " -text {Weight:}")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
        moduleLabel = ".moduledialog.weight"
        tclEval(script = "ttk::label " & moduleLabel)
        tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      else:
        moduleLabel = ".moduledialog.weight"
      if weight > playerShip.modules[shipModuleIndex].weight:
        tclEval(script = moduleLabel & " configure -text {" & $weight & " kg (heavier)} -style Golden.TLabel")
      elif weight < playerShip.modules[shipModuleIndex].weight:
        tclEval(script = moduleLabel & " configure -text {" & $weight & " kg (lighter)} -style Golden.TLabel")
      else:
        tclEval(script = moduleLabel & " configure -text {" & $weight & " kg} -style Golden.TLabel")
      row.inc
  if installing and shipModuleIndex > -1:
    if newInfo:
      moduleLabel = ".moduledialog.durabilitylbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Durability: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.durability"
      tclEval(script = "ttk::label " & moduleLabel)
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
      row.inc
    else:
      moduleLabel = ".moduledialog.durability"
    try:
      if playerShip.modules[shipModuleIndex].maxDurability > modulesList[
          moduleIndex].durability:
        tclEval(script = moduleLabel & " configure -text {weaker} -style Headerred.TLabel")
      elif playerShip.modules[shipModuleIndex].maxDurability < modulesList[
          moduleIndex].durability:
        tclEval(script = moduleLabel & " configure -text {stronger} -style Headergreen.TLabel")
      else:
        discard tclEval(script = moduleLabel & " configure -text {same} -style Golden.TLabel")
    except:
      showError(message = "Can't show module durability")
      return
  if installing:
    if newInfo:
      moduleLabel = ".moduledialog.repairlbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Repair/Upgrade material: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.repair"
      tclEval(script = "ttk::label " & moduleLabel & " -style Golden.TLabel")
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
    else:
      moduleLabel = ".moduledialog.repair"
    var
      mAmount = 0
      infoText = ""
    for item in itemsList.values:
      try:
        if item.itemType == modulesList[moduleIndex].repairMaterial:
          if mAmount > 0:
            infoText.add(y = "{ or }")
          infoText.add(y = item.name)
          mAmount.inc
      except:
        showError(message = "Can't show repair material")
        return
    tclEval(script = moduleLabel & " configure -text {" & infoText & "} -style Golden.TLabel")
    row.inc
    if newInfo:
      moduleLabel = ".moduledialog.repair2lbl"
      tclEval(script = "ttk::label " & moduleLabel & " -text {Repair/Upgrade skill: }")
      tclEval(script = "grid " & moduleLabel & " -sticky w -padx {5 0}")
      moduleLabel = ".moduledialog.repair2"
      tclEval(script = "ttk::label " & moduleLabel & " -style Golden.TLabel")
      tclEval(script = "grid " & moduleLabel & " -sticky w -column 1 -row " & $row)
    else:
      moduleLabel = ".moduledialog.repair2"
    try:
      discard tclEval(script = moduleLabel & " configure -text {" & skillsList[
          modulesList[moduleIndex].repairSkill].name & "/" & attributesList[
          skillsList[modulesList[moduleIndex].repairSkill].attribute].name & "}")
    except:
      showError(message = "Can't show repair skill")
      return
    row.inc
    try:
      if modulesList[moduleIndex].unique:
        moduleLabel = ".moduledialog.unique"
        tclEval(script = "ttk::label " & moduleLabel & " -text {The module is unique. Only one module of that type can be installed on the ship.} -style Golden.TLabel -wraplength 450")
        tclEval(script = "grid " & moduleLabel & " -sticky w -padx 6 -columnspan 2")
    except:
      showError(message = "Can't show module unique info")
      return
    try:
      if modulesList[moduleIndex].description.len > 0:
        if newInfo:
          moduleLabel = ".moduledialog.description"
          tclEval(script = "ttk::label " & moduleLabel & " -text {" &
              modulesList[moduleIndex].description & "} -wraplength 450")
          tclEval(script = "grid " & moduleLabel & " -sticky w -padx 5 -pady {20 0} -columnspan 2")
        else:
          moduleLabel = ".moduledialog.description"
          tclEval(script = moduleLabel & " configure -text {" & modulesList[
              moduleIndex].description & "} -wraplength 450")
    except:
      showError(message = "Can't show module's description")
      return

proc showInstallInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show information about the selected module to install
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowInstallInfo
  moduleIndex = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't set the module index.")
  var
    moduleIterator = 0
    compareModules = ""
  try:
    for module in playerShip.modules:
      if modulesList[module.protoIndex].mType == modulesList[moduleIndex].mType:
        moduleIterator.inc
        compareModules.add(y = "{" & module.name & "} ")
  except:
    return showError(message = "Can't set module iterator.")
  let moduleDialog = try:
      createDialog(name = ".moduledialog", title = modulesList[
          moduleIndex].name, columns = 2)
    except:
      return showError(message = "Can't create the dialog.")
  var row: Positive = 1
  if moduleIterator > 1:
    let compareFrame = moduleDialog & ".compare"
    tclEval(script = "ttk::frame " & compareFrame)
    let compareBox = compareFrame & ".combo"
    tclEval(script = "ttk::combobox " & compareBox &
        " -state readonly -values {" & compareModules & "}")
    tclEval(script = compareBox & " current 0")
    let compareLabel = compareFrame & ".label"
    tclEval(script = "ttk::label " & compareLabel & " -text {Compare with:}")
    tclEval(script = "grid " & compareLabel & " -padx {0 5}")
    tclEval(script = "grid " & compareBox & " -row 0 -column 1 -padx {5 0}")
    tclEval(script = "grid " & compareFrame & " -pady {0 5} -columnspan 2")
    tclEval(script = "bind " & compareBox & " <<ComboboxSelected>> {CompareModules}")
    row = 2
  var cost = try:
      modulesList[moduleIndex].price
    except:
      return showError(message = "Can't set the cost.")
  try:
    countPrice(price = cost, traderIndex = findMember(order = talk))
  except:
    return showError(message = "Can't count the cost.")
  var moduleLabel = moduleDialog & ".costlbl"
  tclEval(script = "ttk::label " & moduleLabel & " -text {Install cost:}")
  tclEval(script = "grid " & moduleLabel & " -sticky w -padx 5 -pady {5 0}")
  moduleLabel = moduleDialog & ".cost"
  tclEval(script = "ttk::label " & moduleLabel)
  tclEval(script = "grid " & moduleLabel &
      " -sticky w -padx {0 5} -pady {5 0} -row " & $row & " -column 1")
  row.inc
  moduleLabel = moduleDialog & ".timelbl"
  tclEval(script = "ttk::label " & moduleLabel & " -text {Install time:}")
  tclEval(script = "grid " & moduleLabel & " -sticky w -padx 5 -pady {5 0}")
  moduleLabel = moduleDialog & ".time"
  tclEval(script = "ttk::label " & moduleLabel)
  tclEval(script = "grid " & moduleLabel &
      " -sticky w -padx {0 5} -pady {5 0} -row " & $row & " -column 1")
  setModuleInfo(installing = true, row = row)
  let
    moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    errorLabel = moduleDialog & ".errorLabel"
    frame = moduleDialog & ".buttonbox"
  tclEval(script = "ttk::frame " & frame)
  let installButton = moduleDialog & ".buttonbox.install"
  tclEval(script = "ttk::button " & installButton &
      " -text Install -image buyicon -style Dialoggreen.TButton -command {CloseDialog " &
      moduleDialog & ";ManipulateModule install}")

  proc setInstallButton(eLabel: string; mIndex2, cost2: Natural) {.sideEffect,
      raises: [KeyError], tags: [].} =
    var
      maxSize, usedSpace, allSpace = 0
      freeTurretIndex = -1
    for index, module in playerShip.modules:
      case module.mType
      of hull:
        maxSize = modulesList[module.protoIndex].value
        usedSpace = module.installedModules
        allSpace = module.maxModules
      of turret:
        if module.gunIndex == -1 and modulesList[module.protoIndex].size >=
            modulesList[moduleIndex].size:
          freeTurretIndex = index
      else:
        discard
    var hasUnique = false
    for module in playerShip.modules:
      if modulesList[module.protoIndex].mType == modulesList[
          moduleIndex].mType and modulesList[moduleIndex].unique:
        hasUnique = true
        break
    if mIndex2 == -1:
      tclEval(script = eLabel & " configure -text {You don't have any money to buy the module.}")
    else:
      if playerShip.cargo[mIndex2].amount < cost2:
        tclEval(script = eLabel & " configure -text {You don't have enough money to buy the module.}")
      elif hasUnique:
        tclEval(script = eLabel & " configure -text {Only one module of that type can be installed on the ship.}")
      elif modulesList[moduleIndex].mType notin {ModuleType.gun, harpoonGun, hull}:
        if modulesList[moduleIndex].size > maxSize:
          tclEval(script = eLabel & " configure -text {The selected module is too big for your's ship's hull.}")
        elif allSpace - usedSpace < modulesList[moduleIndex].size and
            modulesList[moduleIndex].mType != ModuleType.armor:
          tclEval(script = eLabel & " configure -text {You don't have enough space in your ship's hull to install the module.}")
        elif modulesList[moduleIndex].mType == ModuleType.hull and modulesList[
            moduleIndex].maxValue < usedSpace:
          tclEval(script = eLabel & " configure -text {The selected hull is too small to replace your current hull.}")
        elif modulesList[moduleIndex].mType in {ModuleType.gun, harpoonGun} and
            freeTurretIndex == -1:
          tclEval(script = eLabel & " configure -text {You don't have a free turret to install the selected gun.}")

  tclEval(script = "ttk::label " & errorLabel & " -style Headerred.TLabel -wraplength 450 -text {}")
  try:
    setInstallButton(eLabel = errorLabel, mIndex2 = moneyIndex2, cost2 = cost)
  except:
    return showError(message = "Can't set install button.")
  if tclEval2(script = errorLabel & " cget -text") == "":
    tclEval(script = "grid " & installButton & " -padx {0 5}")
    addCloseButton(name = moduleDialog & ".buttonbox.button", text = "Cancel",
        command = "CloseDialog " & moduleDialog, column = 1, icon = "exiticon", color = "red")
  else:
    tclEval(script = "grid " & errorLabel & " -padx 5 -columnspan 2 -sticky w")
    addCloseButton(name = moduleDialog & ".buttonbox.button", text = "Close",
        command = "CloseDialog " & moduleDialog, column = 1, icon = "exiticon")
  tclEval(script = "grid " & frame & " -pady {0 5} -columnspan 2")
  let closeButton = moduleDialog & ".buttonbox.button"
  tclEval(script = "focus " & closeButton)
  tclEval(script = "bind " & closeButton & " <Tab> {focus " & installButton & ";break}")
  tclEval(script = "bind " & moduleDialog & " <Escape> {" & closeButton & " invoke;break}")
  tclEval(script = "bind " & closeButton & " <Escape> {" & closeButton & " invoke;break}")
  showDialog(dialog = moduleDialog, relativeX = 0.25, relativeY = 0.15)
  return tclOk

proc manipulateModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  try:
    if argv[1] == "install":
      upgradeShip(install = true, moduleIndex = moduleIndex)
    else:
      upgradeShip(install = false, moduleIndex = moduleIndex - 1)
      tclEval(script = "SortShipyardModules remove 0 {} 10")
    updateMessages()
    return showShipyardCommand(clientData = clientData, interp = interp,
        argc = 2, argv = @["ShowShipyard", "0"].allocCStringArray)
  except NoMoneyError:
    showMessage(text = "You don't have " & moneyName & " to pay for modules.",
        title = "Can't install module.")
  except NotEnoughMoneyError:
    showMessage(text = "You don't have enough " & moneyName & " to pay for " &
        getCurrentExceptionMsg() & ".", title = "Can't install module.")
  except UniqueModuleError:
    showMessage(text = "You can't install another " & getCurrentExceptionMsg() &
        " because you have installed one module of that type. Remove the old first.",
        title = "Can't install module.")
  except InstallationError, RemovingError:
    showMessage(text = getCurrentExceptionMsg(), title = "Can't " & (if argv[
        1] == "install": "install" else: "remove") & " module.")
  except NoFreeCargoError:
    showMessage(text = "You don't have enough free space for " & moneyName &
        " in ship cargo.", title = "Can't remove module")
  except NoMoneyInBaseError:
    showMessage(text = "Base don't haev enough " & moneyName &
        " for buy this module.", title = "Can't remove module")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowShipyard", showShipyardCommand)
#    addCommand("ShowInstallInfo", showInstallInfoCommand)
#    addCommand("ManipulateModule", manipulateModuleCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc setAdaModuleInfo(installing: cint; row: var cint;
    newInfo, mIndex: cint) {.exportc.} =
  moduleIndex = mIndex
  var newRow = row.Positive
  setModuleInfo(installing = installing == 1, row = newRow, newInfo = newInfo == 1)
  row = newRow.cint
