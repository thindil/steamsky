# Copyright 2022-2024 Bartek thindil Jasicki
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
import ../[bases, config, crew2, crewinventory, events2, game, game2,
    maps, messages, missions2, shipscargo, shipscrew, tk, types]
import combatui, coreui, dialogs, mapsui, shipsuicrew, shipsuimodules2

proc resizeCanvasCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Resize the selected canvas
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the canvas was resized, otherwise tclError
  ##
  ## Tcl:
  ## ResizeCanvas name width height
  ## Name is the name of the canvas to resize, width it a new width, height
  ## is a new height
  let canvas = $argv[1]
  if tclEval2(script = "winfo exists " & canvas) == "0":
    return tclOk
  let parentFrame = tclEval2(script = "winfo parent " & canvas)
  tclEval(script = "bind " & parentFrame & " <Configure>")
  tclEval(script = canvas & " configure -width " & $argv[2] &
      " -height [expr " & $argv[3] & " - 20]")
  tclEval(script = "bind " & parentFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  return tclOk

proc checkAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Check the amount of the item, if it is not below low level of warning or
  ## if the entered amount is a proper number
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the amount is valid, otherwise tclError
  ##
  ## Tcl:
  ## CheckAmount name cargoindex value action button
  ## Name is the name of spinbox which value will be checked, cargoindex is
  ## the index of the item in the cargo, value is the value entered by the
  ## player, action is the action performed by the player and button is
  ## the button which accept the action
  try:
    var value = 0
    if argv[3].len > 0:
      try:
        let val = $argv[3]
        value = val.parseInt
      except:
        tclSetResult("0")
        return tclOk
    var warningText = ""
    if $argv[1] == ".itemdialog.giveamount":
      warningText = "You will give amount below low lewel of "
    else:
      warningText = "You will " & $argv[4] & " amount below low lewel of "
    let
      spinBox = $argv[1]
      maxValue = tclEval2(script = spinBox & " cget -to").parseInt
    let button = $argv[argc - 1]
    if value < 1:
      if button.len > 0:
        tclEval(script = button & " configure -state disabled")
      tclSetResult("1")
      return tclOk
    elif value > maxValue:
      tclEval(script = spinBox & " set " & $maxValue)
      value = maxValue
    if button.len > 0:
      tclEval(script = button & " configure -state normal")
    if argc > 4:
      if argv[4] == "take":
        tclSetResult("1")
        return tclOk
      elif $argv[4] in ["buy", "sell"]:
        var cost: Natural = value * parseInt(s = $argv[5])
        countPrice(price = cost, traderIndex = findMember(order = talk),
            reduce = argv[4] == "buy")
        let label = ".itemdialog.cost2lbl"
        tclEval(script = label & " configure -text {" & $cost & " " &
            moneyName & "}")
        if argv[4] == "buy":
          tclSetResult("1")
          return tclOk
    let
      label = ".itemdialog.errorlbl"
      cargoIndex = parseInt(s = $argv[2]) - 1
    if itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType == fuelType:
      let amount = getItemAmount(itemType = fuelType) - value
      if amount <= gameSettings.lowFuel:
        tclEval(script = label & " configure -text {" & warningText & "fuel.}")
        tclEval(script = "grid " & label)
        tclSetResult("1")
        return tclOk
    for member in playerShip.crew:
      let faction = factionsList[member.faction]
      if itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType in
          faction.drinksTypes:
        let amount = getItemsAmount(iType = "Drinks") - value
        if amount <= gameSettings.lowDrinks:
          tclEval(script = label & " configure -text {" & warningText & "drinks.}")
          tclEval(script = "grid " & label)
          tclSetResult("1")
          return tclOk
      elif itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType in
          faction.foodTypes:
        let amount = getItemsAmount(iType = "Food") - value
        if amount <= gameSettings.lowFood:
          tclEval(script = label & " configure -text {" & warningText & "food.}")
          tclEval(script = "grid " & label)
          tclSetResult("1")
          return tclOk
    tclEval(script = "grid remove " & label)
    tclSetResult("1")
    return tclOk
  except:
    tclSetResult("0")
    return tclError

proc validateAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Validate amount of the item when the spinbox button to increase or
  ## decrease the amount was pressed
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the amount is valid, otherwise tclError
  ##
  ## Tcl:
  ## ValidateAmount name
  ## Name is the name of spinbox which value will be validated
  let value = tclEval2(script = $argv[1] & " get")
  var newArgv: seq[string]
  for i in 0 ..< argc:
    newArgv.add($argv[i])
  newArgv.insert(value, 3)
  return checkAmountCommand(clientData, interp, newArgv.len.cint,
      newArgv.allocCStringArray)

proc setTextVariableCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Set the player's ship, module or crew member's name in Nim and Tcl
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the name was set, otherwise tclError
  ##
  ## Tcl:
  ## SetTextVariable variablename
  ## Variablename is the name of variable to set
  let
    varName = $argv[1]
    tEntry = ".getstring.entry"
    value = tclEval2(script = tEntry & " get")
  tclSetVar(varName, value)
  if varName == "shipname":
    playerShip.name = value
  elif varName.len > 10 and varName[0 .. 9] == "modulename":
    let moduleIndex = try:
        varName[10 .. ^1].parseInt
      except ValueError:
        -1
    if moduleIndex == -1:
      return tclError
    playerShip.modules[moduleIndex - 1].name = value
    tclUnsetVar(varName)
    updateModulesInfo()
  elif varName.len > 8 and varName[0 .. 7] == "crewname":
    let crewIndex = try:
        varName[8 .. ^1].parseInt
      except ValueError:
        -1
    if crewIndex == -1:
      return tclError
    playerShip.crew[crewIndex - 1].name = value
    tclUnsetVar(varName)
    updateCrewInfo()
  return tclOk

proc processQuestionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  let answer = argv[1]
  if answer == "deletesave":
    removeFile(saveDirectory & tclGetVar("deletesave"))
    tclUnsetVar("deletesave")
    tclEval(script = "ShowLoadGame")
  elif answer == "sethomebase":
    let moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    if moneyIndex2 == -1:
      showMessage(text = "You don't have any " & moneyName &
          " for change ship home base.", title = "No money")
      return tclOk
    let traderIndex = findMember(order = talk)
    var price: Natural = 1_000
    countPrice(price = price, traderIndex = traderIndex)
    if playerShip.cargo[moneyIndex2].amount < price:
      showMessage(text = "You don't have enough " & moneyName &
          " for change ship home base.", title = "No money")
      return tclOk
    playerShip.homeBase = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = -price)
    addMessage(message = "You changed your ship home base to: " & skyBases[
        playerShip.homeBase].name, mType = otherMessage)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    updateGame(minutes = 10)
    showSkyMap()
  elif answer == "nopilot":
    waitForRest()
    let startsCombat = checkForEvent()
    var message: string
    if not startsCombat and gameSettings.autoFinish:
      message = autoFinishMissions()
    if message.len > 0:
      showMessage(text = message, title = "Error")
    centerX = playerShip.skyX
    centerY = playerShip.skyY
    if startsCombat:
      showCombatUi()
    else:
      showSkyMap()
  elif answer == "quit":
    gameSettings.messagesPosition = gameSettings.windowHeight - tclEval2(
        script = mainPaned & " sashpos 0").parseInt
    endGame(save = true)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Add Tcl commands related to the various UI elements
  discard
#  addCommand("ResizeCanvas", resizeCanvasCommand)
#  addCommand("CheckAmount", checkAmountCommand)
#  addCommand("ValidateAmount", validateAmountCommand)
#  addCommand("NimSetTextVariable", setTextVariableCommand)

# Temporary code for interfacing with Ada

proc addAdaUtilsCommands() {.raises: [], tags: [], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

proc deleteAdaWidgets*(startIndex, endIndex: cint; frame: cstring) {.exportc,
    gcsafe, sideEffect, raises: [], tags: [].} =
  deleteWidgets(startIndex, endIndex, $frame)
