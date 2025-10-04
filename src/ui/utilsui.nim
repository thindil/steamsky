# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides various procedures related to the game's UI, like resizing canvas,
## validating the player's input, showing on the map, etc.

import std/[os, strutils, tables]
import contracts, nimalyzer
import ../[bases, config, crew2, events2, game, game2, items, maps, messages,
    missions2, shipscargo, shipscrew, shipscrew2, tk, types, utils]
import combatui, coreui, dialogs, errordialog, mapsui, shipsuicrew,
    shipsuimodules2, showmainmenu, statisticsui, updateheader, utilsui2

proc resizeCanvasCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], contractual, cdecl,
    ruleOff: "params".} =
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
  let canvas: string = $argv[1]
  if tclEval2(script = "winfo exists " & canvas) == "0":
    return tclOk
  let parentFrame: string = tclEval2(script = "winfo parent " & canvas)
  tclEval(script = "bind " & parentFrame & " <Configure>")
  tclEval(script = canvas & " configure -width " & $argv[2] &
      " -height [expr " & $argv[3] & " - 20]")
  tclEval(script = "bind " & parentFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  return tclOk

proc checkAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], contractual, cdecl.} =
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
    var value: int = 0
    if argv[3].len > 0:
      try:
        let val: string = $argv[3]
        value = val.parseInt
      except:
        tclSetResult(value = "0")
        return tclOk
    var warningText: string = ""
    if $argv[1] == ".itemdialog.giveamount":
      warningText = "You will give amount below low lewel of "
    else:
      warningText = "You will " & $argv[4] & " amount below low lewel of "
    let
      spinBox: string = $argv[1]
      maxValue: int = tclEval2(script = spinBox & " cget -to").parseInt
    let button: string = $argv[argc - 1]
    if value < 1:
      if button.len > 0:
        tclEval(script = button & " configure -state disabled")
      tclSetResult(value = "1")
      return tclOk
    elif value > maxValue:
      tclEval(script = spinBox & " set " & $maxValue)
      value = maxValue
    if button.len > 0:
      tclEval(script = button & " configure -state normal")
    let cargoIndex: int = parseInt(s = $argv[2]) - 1
    const label: string = ".itemdialog.errorlbl"
    if argc > 4:
      if argv[4] == "take":
        tclSetResult(value = "1")
        return tclOk
      elif $argv[4] in ["buy", "sell"]:
        var cost: Natural = value * parseInt(s = $argv[5])
        countPrice(price = cost, traderIndex = findMember(order = talk),
            reduce = argv[4] == "buy")
        const costLabel: string = ".itemdialog.cost2lbl"
        tclEval(script = costLabel & " configure -text {" & $cost & " " &
            moneyName & "}")
        if argv[4] == "buy":
          if getItemAmount(itemType = fuelType) - cost <= gameSettings.lowFuel:
            tclEval(script = label & " configure -text {You will spend " &
                moneyName & " below low level of fuel.}")
            tclEval(script = "grid " & label)
          else:
            tclEval(script = "grid remove " & label)
          tclSetResult(value = "1")
          return tclOk
    if itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType == fuelType:
      let amount: int = getItemAmount(itemType = fuelType) - value
      if amount <= gameSettings.lowFuel:
        tclEval(script = label & " configure -text {" & warningText & "fuel.}")
        tclEval(script = "grid " & label)
        tclSetResult(value = "1")
        return tclOk
    for member in playerShip.crew:
      let faction: FactionData = factionsList[member.faction]
      if itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType in
          faction.drinksTypes:
        let amount: int = getItemsAmount(iType = "Drinks") - value
        if amount <= gameSettings.lowDrinks:
          tclEval(script = label & " configure -text {" & warningText & "drinks.}")
          tclEval(script = "grid " & label)
          tclSetResult(value = "1")
          return tclOk
      elif itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType in
          faction.foodTypes:
        let amount: int = getItemsAmount(iType = "Food") - value
        if amount <= gameSettings.lowFood:
          tclEval(script = label & " configure -text {" & warningText & "food.}")
          tclEval(script = "grid " & label)
          tclSetResult(value = "1")
          return tclOk
    tclEval(script = "grid remove " & label)
    tclSetResult(value = "1")
    return tclOk
  except:
    tclSetResult(value = "0")
    return tclError

proc validateAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], contractual, cdecl.} =
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
  let value: string = tclEval2(script = $argv[1] & " get")
  var newArgv: seq[string] = @[]
  for i in 0 ..< argc:
    newArgv.add(y = $argv[i])
  newArgv.insert(item = value, i = 3)
  return checkAmountCommand(clientData = clientData, interp = interp,
      argc = newArgv.len.cint, argv = newArgv.allocCStringArray)

proc setTextVariableCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], contractual, cdecl.} =
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
  const tEntry: string = ".getstring.entry"
  let
    varName: string = $argv[1]
    value: string = tclEval2(script = tEntry & " get")
  tclSetVar(varName = varName, newValue = value)
  if varName == "shipname":
    playerShip.name = value
  elif varName.len > 10 and varName[0..9] == "modulename":
    let moduleIndex: int = try:
        varName[10..^1].parseInt
      except ValueError:
        -1
    if moduleIndex == -1:
      return tclError
    playerShip.modules[moduleIndex - 1].name = value
    tclUnsetVar(varName = varName)
    updateModulesInfo()
  elif varName.len > 8 and varName[0..7] == "crewname":
    let crewIndex: int = try:
        varName[8..^1].parseInt
      except ValueError:
        -1
    if crewIndex == -1:
      return tclError
    playerShip.crew[crewIndex - 1].name = value
    tclUnsetVar(varName = varName)
    updateCrewInfo()
  return tclOk

proc showOnMapCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual, cdecl.} =
  ## Show the selected point on map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the name was set, otherwise tclError
  ##
  ## Tcl:
  ## ShowOnMap X Y
  ## X is the x coordinate of point to show, Y is the y coordinate of point
  ## to show
  centerX = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't set center X.")
  centerY = try:
      ($argv[2]).parseInt
    except:
      return showError(message = "Can't set center Y.")
  tclEval(script = "InvokeButton " & closeButton)
  tclEval(script = "grid remove " & closeButton)
  return tclOk

proc processQuestionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect], contractual, cdecl.} =
  ## Process question from dialog when the player answer Yes there
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the name was set, otherwise tclError
  ##
  ## Tcl:
  ## ProcessQuestion answer
  ## Answer is the answer set for the selected question
  let answer: cstring = argv[1]
  case answer
  of "deletesave":
    try:
      removeFile(file = tclGetVar(varName = "deletesave"))
    except:
      return showError(message = "Can't remove the save file.")
    tclUnsetVar(varName = "deletesave")
    tclEval(script = "ShowLoadGame")
  of "sethomebase":
    let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
    if moneyAmount == 0:
      showMessage(text = "You don't have any " & moneyName &
          " for change ship home base.", title = "No money")
      return tclOk
    let traderIndex: int = findMember(order = talk)
    var price: Natural = 1_000
    try:
      countPrice(price = price, traderIndex = traderIndex)
    except:
      return showError(message = "Can't count price.")
    if moneyAmount < price:
      showMessage(text = "You don't have enough " & moneyName &
          " for change ship home base.", title = "No money")
      return tclOk
    playerShip.homeBase = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    try:
      updateMoney(memberIndex = -1, amount = -price, quality = any)
    except:
      return showError(message = "Can't update the player's money.")
    addMessage(message = "You changed your ship home base to: " & skyBases[
        playerShip.homeBase].name, mType = otherMessage)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    try:
      updateGame(minutes = 10)
    except:
      return showError(message = "Can't update the game.")
    showSkyMap()
  of "nopilot":
    try:
      waitForRest()
    except:
      return showError(message = "Can't wait for rest.")
    let startsCombat: bool = try:
        checkForEvent()
      except:
        return showError(message = "Can't start a combat.")
    var message: string = ""
    if not startsCombat and gameSettings.autoFinish:
      message = try:
          autoFinishMissions()
        except:
          return showError(message = "Can't autofinish mission.")
    if message.len > 0:
      showMessage(text = message, title = "Error")
    centerX = playerShip.skyX
    centerY = playerShip.skyY
    if startsCombat:
      showCombatUi()
    else:
      showSkyMap()
  of "quit":
    gameSettings.messagesPosition = try:
        gameSettings.windowHeight - tclEval2(script = mainPaned &
            " sashpos 0").parseInt
      except:
        return showError(message = "Can't set messages position.")
    try:
      endGame(save = true)
    except:
      return showError(message = "Can't end the game.")
    showMainMenu()
  of "resign":
    try:
      death(memberIndex = 0, reason = "resignation", ship = playerShip)
    except:
      return showError(message = "Can't kill the player.")
    showQuestion(question = "You are dead. Would you like to see your game statistics?",
        res = "showstats")
  of "showstats":
    let button: string = gameHeader & ".menubutton"
    tclEval(script = "grid " & button)
    tclEval(script = closeButton & " configure -command ShowMainMenu")
    tclEval(script = "grid " & closeButton & " -row 0 -column 1")
    tclSetVar(varName = "gamestate", newValue = "dead")
    showStatistics()
    try:
      endGame(save = false)
    except:
      return showError(message = "Can't end the game2.")
  of "mainmenu":
    gameSettings.messagesPosition = try:
        gameSettings.windowHeight - tclEval2(script = mainPaned &
            " sashpos 0").parseInt
      except:
        return showError(message = "Can't set messages position.")
    try:
      endGame(save = false)
    except:
      return showError(message = "Can't end the game3.")
    showMainMenu()
  of "messages":
    let typeBox: string = mainPaned & ".messagesframe.canvas.messages.options.types"
    clearMessages()
    tclEval(script = typeBox & " current 0")
    tclEval(script = "ShowLastMessages")
  of "retire":
    try:
      death(memberIndex = 0, reason = "retired after finished the game",
          ship = playerShip)
    except:
      return showError(message = "Can't kill the player2.")
    showQuestion(question = "You are dead. Would you like to see your game statistics?",
        res = "showstats")
  else:
    let
      baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
          playerShip.skyY].baseIndex
      memberIndex: int = try:
          ($argv[1]).parseInt - 1
        except:
          return showError(message = "Can't get the member index.")
    addMessage(message = "You dismissed " & playerShip.crew[memberIndex].name &
        ".", mType = orderMessage)
    try:
      deleteMember(memberIndex = memberIndex, ship = playerShip)
    except:
      return showError(message = "Can't delete the member.")
    skyBases[baseIndex].population.inc
    for index, _ in playerShip.crew:
      try:
        updateMorale(ship = playerShip, memberIndex = index, value = getRandom(
            min = -5, max = -1))
      except:
        return showError(message = "Can't update the crew's morale.")
    updateCrewInfo()
    updateHeader()
    updateMessages()
  return tclOk

proc setScrollbarBindingsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], contractual, cdecl.} =
  ## Assign scrolling events with mouse wheel to the selected vertical
  ## scrollbar from the selected widget
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the name was set, otherwise tclError
  ##
  ## Tcl:
  ## SetScrollbarBindings widget scrollbar
  ## Widget is the widget from which events will be fired, scrollbar is
  ## Ttk::scrollbar which to which bindings will be added
  let
    widget: string = $argv[1]
    scrollbar: string = $argv[2]
  tclEval(script = "bind " & widget & " <Button-4> {if {[winfo ismapped " &
      scrollbar & "]} {event generate " & scrollbar & " <Button-4>}}")
  tclEval(script = "bind " & widget & " <Key-Prior> {if {[winfo ismapped " &
      scrollbar & "]} {event generate " & scrollbar & " <Button-4>}}")
  tclEval(script = "bind " & widget & " <Button-5> {if {[winfo ismapped " &
      scrollbar & "]} {event generate " & scrollbar & " <Button-5>}}")
  tclEval(script = "bind " & widget & " <Key-Next> {if {[winfo ismapped " &
      scrollbar & "]} {event generate " & scrollbar & " <Button-5>}}")
  tclEval(script = "bind " & widget & " <MouseWheel> {if {[winfo ismapped " &
      scrollbar & "]} {event generate " & scrollbar & " <MouseWheel> -delta %D}}")
  return tclOk

proc setDestination2Command(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual, cdecl.} =
  ## Set the selected map point as the player's ship destination
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the name was set, otherwise tclError
  ##
  ## Tcl:
  ## SetDestination2 X Y
  ## X is the x coordinate of point to set, Y is the y coordinate of point
  ## to set
  let
    posX: int = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the X position.")
    posY: int = try:
        ($argv[2]).parseInt
      except:
        return showError(message = "Can't get the Y position.")
  if posX == playerShip.skyX and posY == playerShip.skyY:
    showMessage(text = "You are at this location now.",
        title = "Can't set destination")
    return tclOk
  playerShip.destinationX = posX
  playerShip.destinationY = posY
  addMessage(message = "You set the travel destination for your ship.",
      mType = orderMessage)
  tclEval(script = "InvokeButton " & closeButton)
  tclEval(script = "grid remove " & closeButton)
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Add Tcl commands related to the various UI elements
  try:
    addCommand(name = "ResizeCanvas", nimProc = resizeCanvasCommand)
    addCommand(name = "CheckAmount", nimProc = checkAmountCommand)
    addCommand(name = "ValidateAmount", nimProc = validateAmountCommand)
    addCommand(name = "SetTextVariable", nimProc = setTextVariableCommand)
    addCommand(name = "ShowOnMap", nimProc = showOnMapCommand)
    addCommand(name = "ProcessQuestion", nimProc = processQuestionCommand)
    addCommand(name = "SetScrollbarBindings",
        nimProc = setScrollbarBindingsCommand)
    addCommand(name = "SetDestination2", nimProc = setDestination2Command)
  except:
    showError(message = "Can't add a Tcl command.")
