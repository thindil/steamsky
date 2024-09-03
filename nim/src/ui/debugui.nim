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
import ../[basestypes, game, gamesaveload, items, maps, tk]
import mapsui

proc refreshModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the information about selected module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshModule
  let
    frameName = ".debugdialog.main.ship"
    moduleCombo = frameName & ".module"
    moduleIndex = try:
        tclEval2(script = moduleCombo & " current").parseInt
      except:
        return showError(message = "Can't get the module's index.")
    protoCombo = frameName & ".proto"
  try:
    tclEval(script = protoCombo & " set {" & modulesList[playerShip.modules[
        moduleIndex].protoIndex].name & "}")
  except:
    return showError(message = "Can't get the proto module.")
  var spinBox = frameName & ".weight"
  tclEval(script = spinBox & " set " & $playerShip.modules[moduleIndex].weight)
  spinBox = frameName & ".dur"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].durability)
  spinBox = frameName & ".maxdur"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].maxDurability)
  spinBox = frameName & ".upgrade"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].upgradeProgress)
  return tclOk

proc refreshMemberCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the information about selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshMember
  let frameName = ".debugdialog.main.crew"
  var comboBox = frameName & ".member"
  let
    memberIndex = try:
        tclEval2(script = comboBox & " current").parseInt
      except:
        return showError(message = "Can't get the member index.")
    member = playerShip.crew[memberIndex]
  var spinBox = frameName & ".stats2.health"
  tclEval(script = spinBox & " set " & $member.health)
  spinBox = frameName & ".stats2.thirst"
  tclEval(script = spinBox & " set " & $member.thirst)
  spinBox = frameName & ".stats2.hunger"
  tclEval(script = spinBox & " set " & $member.hunger)
  spinBox = frameName & ".stats2.tired"
  tclEval(script = spinBox & " set " & $member.tired)
  spinBox = frameName & ".stats2.morale"
  tclEval(script = spinBox & " set " & $member.morale[1])
  spinBox = frameName & ".stats2.loyalty"
  tclEval(script = spinBox & " set " & $member.loyalty)
  var
    memberFrame = frameName & ".stats"
    rows = try:
        tclEval2(script = "grid size " & memberFrame).split[1].parseInt
      except:
        return showError(message = "Can't get the amount of rows.")
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = memberFrame)
  for index, attribute in member.attributes:
    let label = memberFrame & ".label" & $(index + 1)
    tclEval(script = "ttk::label " & label & " -text {" & attributesList[
        index].name & "}")
    tclEval(script = "grid " & label)
    spinBox = memberFrame & ".value" & $(index + 1)
    tclEval(script = "ttk::spinbox " & spinBox &
        " -from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P " &
        frameName & ".change} -width 5")
    tclEval(script = spinBox & " set " & $attribute.level)
    tclEval(script = "grid " & spinBox & " -column 1 -row " & $(index + 1))
  memberFrame = frameName & ".skills"
  rows = try:
      tclEval2(script = "grid size " & memberFrame).split[1].parseInt
    except:
      return showError(message = "Can't get the amount of rows (2).")
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = memberFrame)
  var skillsIndexes: seq[Natural]
  for index, skill in member.skills:
    let label = memberFrame & ".label" & $(index + 1)
    try:
      tclEval(script = "ttk::label " & label & " -text {" & skillsList[
          skill.index].name & "}")
    except:
      return showError(message = "Can't add the skill label.")
    tclEval(script = "grid " & label)
    spinBox = memberFrame & ".value" & $(index + 1)
    tclEval(script = "ttk::spinbox " & spinBox &
        " -from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P " &
        frameName & ".change} -width 5")
    tclEval(script = spinBox & " set " & $skill.level)
    tclEval(script = "grid " & spinBox & " -column 1 -row " & $(index + 1))
    skillsIndexes.add(y = skill.index)
  var skillsListValues = ""
  for index, skill in skillsList:
    if index notin skillsIndexes:
      skillsListValues.add(y = " " & skill.name)
  comboBox = frameName & ".addskill.skills"
  tclEval(script = comboBox & " configure -values [list" & $skillsListValues & "]")
  tclEval(script = comboBox & " current 0")
  return tclOk

proc refreshCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the information about the player ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshCargo
  let
    frameName = ".debugdialog.main.cargo"
    cargoCombo = frameName & ".update"
    itemIndex = try:
        tclEval2(script = cargoCombo & " current").parseInt
      except:
        return showError(message = "Can't get the item index.")
    amountBox = frameName & ".updateamount"
  tclEval(script = amountBox & " set " & $playerShip.cargo[itemIndex].amount)
  return tclOk

proc refreshEventsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the list of events
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshEvents
  let
    frameName = ".debugdialog.main.world.deleteevent"
    eventsButton = frameName & ".deleteevent"
    eventsBox = frameName & ".delete"
  if eventsList.len == 0:
    tclEval(script = "grid remove " & eventsButton)
    tclEval(script = "grid remove " & eventsBox)
    return tclOk
  tclEval(script = "grid " & eventsButton)
  tclEval(script = "grid " & eventsBox)
  var valuesList = ""
  for index, event in eventsList:
    try:
      case event.eType
      of enemyShip:
        valuesList.add(y = " {Enemy ship: " & protoShipsList[
            event.shipIndex].name & "}")
      of attackOnBase:
        valuesList.add(y = " {Attack on base: " & protoShipsList[
            event.shipIndex].name & "}")
      of disease:
        valuesList.add(y = " {Disease in base: " & skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name & "}")
      of doublePrice:
        valuesList.add(y = " {Double price in base: " & skyBases[skyMap[
            event.skyX][event.skyY].baseIndex].name & "}")
      of fullDocks:
        valuesList.add(y = " {Full docks in base: " & skyBases[skyMap[
            event.skyX][event.skyY].baseIndex].name & "}")
      of enemyPatrol:
        valuesList.add(y = " {Enemy patrol: " & protoShipsList[
            event.shipIndex].name & "}")
      of trader:
        valuesList.add(y = " {Trader: " & protoShipsList[event.shipIndex].name & "}")
      of friendlyShip:
        valuesList.add(y = " {Friendly ship: " & protoShipsList[
            event.shipIndex].name & "}")
      else:
        discard
    except:
      return showError(message = "Can't add event to the list.")
  tclEval(script = eventsBox & " configure -values [list" & $valuesList & "]")
  tclEval(script = eventsBox & " current 0")
  return tclOk

proc refreshCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the whole game information
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Refresh
  let frameName = ".debugdialog.main"
  var spinBox = frameName & ".ship.x"
  tclEval(script = spinBox & " set " & $playerShip.skyX)
  spinBox = frameName & ".ship.y"
  tclEval(script = spinBox & " set " & $playerShip.skyY)
  var valuesList = ""
  for module in playerShip.modules:
    valuesList.add(y = " {" & module.name & "}")
  var comboBox = frameName & ".ship.module"
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshModuleCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  comboBox = frameName & ".crew.member"
  valuesList = ""
  for member in playerShip.crew:
    valuesList.add(y = " {" & member.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshMemberCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  comboBox = frameName & ".cargo.update"
  valuesList = ""
  for item in playerShip.cargo:
    valuesList.add(y = " {" & getItemName(item = item, damageInfo = false,
        toLower = false) & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshCargoCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  discard refreshEventsCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  return tclOk

proc refreshBaseCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the information about the selected base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshBase
  let
    frameName = ".debugdialog.main.bases"
    nameEntry = frameName & ".name"
    baseName = tclEval2(script = nameEntry & " get")
  var baseIndex = 0
  for index, base in skyBases:
    if base.name == baseName:
      baseIndex = index
      break
  if baseIndex == 0:
    return tclOk
  var comboBox = frameName & ".type"
  try:
    tclEval(script = comboBox & " set " & basesTypesList[skyBases[
        baseIndex].baseType].name)
  except:
    return showError(message = "Can't set base's type.")
  comboBox = frameName & ".owner"
  try:
    tclEval(script = comboBox & " set " & factionsList[skyBases[
        baseIndex].owner].name)
  except:
    return showError(message = "Can't set owner's name.")
  comboBox = frameName & ".size"
  tclEval(script = comboBox & " current " & $(skyBases[baseIndex].size.ord))
  var spinBox = frameName & ".population"
  tclEval(script = spinBox & " set " & $skyBases[baseIndex].population)
  spinBox = frameName & ".reputation"
  tclEval(script = spinBox & " set " & $skyBases[baseIndex].reputation.level)
  spinBox = frameName & ".money"
  if skyBases[baseIndex].cargo.len > 0:
    tclEval(script = spinBox & " set " & $skyBases[baseIndex].cargo[0].amount)
  else:
    tclEval(script = spinBox & " set 0")
  return tclOk

proc debugSaveGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Save the game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugSaveGame
  try:
    saveGame(prettyPrint = true)
  except:
    return showError(message = "Can't save the game.")
  return tclOk

proc debugMoveShipCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Move the player ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugMoveShip
  let frameName = ".debugdialog.main.ship"
  var spinBox = frameName & ".x"
  playerShip.skyX = try:
      tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't get X coord.")
  spinBox = frameName & ".y"
  playerShip.skyY = try:
      tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't get Y coord.")
  showSkyMap(clear = true)
  return tclOk

proc debugUpdateModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  let
    frameName = ".debugdialog.main.ship"
    moduleBox = frameName & ".module"
    moduleIndex = try:
        tclEval2(script = moduleBox & " current").parseInt
      except:
        return showError(message = "Can't get module index.")
    protoCombo = frameName & ".proto"
  var value = tclEval2(script = protoCombo & " get")
  for index, module in modulesList:
    if module.name == value:
      value = ""
      playerShip.modules[moduleIndex].protoIndex = index
      break
  var spinBox = frameName & ".weight"
  playerShip.modules[moduleIndex].weight = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set weight.")
  spinBox = frameName & ".dur"
  playerShip.modules[moduleIndex].durability = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set durability.")
  spinBox = frameName & ".maxdur"
  playerShip.modules[moduleIndex].maxDurability = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set max durability.")
  spinBox = frameName & ".upgrade"
  playerShip.modules[moduleIndex].upgradeProgress = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set upgrade progress.")
  return tclOk

proc showDebugUi*() =
  tclEvalFile(fileName = dataDirectory & DirSep & "debug.tcl")
#    addCommand("Refresh", refreshCommand)
#    addCommand("RefreshModule", refreshModuleCommand)
#    addCommand("RefreshMember", refreshMemberCommand)
#    addCommand("RefreshCargo", refreshCargoCommand)
#    addCommand("RefreshEvents", refreshEventsCommand)
#    addCommand("RefreshBase", refreshBaseCommand)
#    addCommand("DebugSaveGame", debugSaveGameCommand)
#    addCommand("DebugMoveShip", debugMoveShipCommand)
#    addCommand("DebugUpdateModule", debugUpdateModuleCommand)
