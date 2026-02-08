# Copyright 2024-2026 Bartek thindil Jasicki
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

## Provides code related to the orders menu in the game, like show the menu,
## execute some orders, etc.

import std/[tables, strutils]
import contracts, nimalyzer
import ../[bases, bases2, basestypes, combat, crewinventory, events, events2,
    game, game2, maps, messages, missions, missions2, shipscargo, shipscrew,
    shipsmovement, stories, stories2, tk, trades, types, utils]
import combatui, coreui, dialogs, dialogs2, errordialog, updateheader, utilsui2

type OrderShortcut = object
  buttonName: string
  shortcut: char

var
  lastButton: string = "."
  shortcuts: seq[OrderShortcut] = @[]

proc addButton(name, label, command, shortcut: string; underline: Natural;
    row: int = -1) {.raises: [], tags: [], contractual.} =
  ## Add a button to the orders menu
  ##
  ## * name      - the neme of the button
  ## * label     - the text on the button
  ## * command   - the Tcl command to execute when the button was pressed
  ## * shortcut  - the keyboard shortcut for the button
  ## * underline - the character which will be underlined in the text
  ##               (for shortcut)
  ## * row       - the row in which the button will be added, default is -1
  ##               which means, the next row
  let button: string = ".gameframe.orders" & name
  tclEval(script = "ttk::button " & button & " -text {" & label &
      "} -command {CloseDialog .gameframe.orders;" & command &
      "} -underline " & $underline)
  tclEval(script = "grid " & button & " -sticky we -padx 5" & (if row ==
      -1: "" else: " -row " & $row))
  tclEval(script = "bind " & button & " <Escape> {.gameframe.orders.closebutton invoke;break}")
  lastButton = button
  shortcuts.add(y = OrderShortcut(buttonName: button, shortcut: shortcut[0]))

proc showDockedCommands(baseIndex: ExtendedBasesRange;
    haveTrader: bool): bool {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the available orders when the player's ship is docked to a base
  ##
  ## * baseIndex  - the index of the base to which the player's ship is docked
  ## * haveTrader - if true, someone in the crew is assigned to trader position
  ##
  ## Returns false if everything was show ok, otherwise show error and return
  ## true
  result = false
  addButton(name = ".undock", label = "Undock", command = "Docking",
      shortcut = "d", underline = 2)
  if getBasePopulation(baseIndex = baseIndex) > empty:
    addButton(name = ".escape", label = "Escape", command = "Docking escape",
        shortcut = "a", underline = 3)
    if haveTrader:
      addButton(name = ".trade", label = "Trade", command = "ShowTrade",
          shortcut = "t", underline = 0)
      addButton(name = ".school", label = "School", command = "ShowSchool",
          shortcut = "s", underline = 0)
      if skyBases[baseIndex].recruits.len > 0:
        addButton(name = ".recruits", label = "Recruit",
            command = "ShowRecruit", shortcut = "r", underline = 0)
    if daysDifference(dateToCompare = skyBases[baseIndex].askedForEvents) > 6:
      addButton(name = ".events", label = "Ask for events",
          command = "AskForEvents", shortcut = "e", underline = 8)
    if not skyBases[baseIndex].askedForBases:
      addButton(name = ".bases", label = "Ask for bases",
          command = "AskForBases", shortcut = "b", underline = 8)
    try:
      if "temple" in basesTypesList[skyBases[baseIndex].baseType].flags:
        addButton(name = ".pray", label = "Pray", command = "Pray",
            shortcut = "p", underline = 0)
    except:
      showError(message = "Can't check if base has temple flag.")
      return true
    for member in playerShip.crew:
      if member.health < 100:
        addButton(name = ".heal", label = "Heal wounded",
            command = "ShowBaseUI heal", shortcut = "w", underline = 5)
        break
    for module in playerShip.modules:
      if module.durability < module.maxDurability:
        addButton(name = ".repair", label = "Repair ship",
            command = "ShowBaseUI repair", shortcut = "p", underline = 2)
        break
    try:
      if "shipyard" in basesTypesList[skyBases[baseIndex].baseType].flags:
        addButton(name = ".shipyard", label = "Shipyard",
            command = "ShowShipyard", shortcut = "i", underline = 2)
    except:
      showError(message = "Can't check if the base has shipyard flag.")
      return true
    for index, recipe in recipesList:
      try:
        if index notin knownRecipes and index in basesTypesList[skyBases[
            baseIndex].baseType].recipes and recipe.reputation <= skyBases[
            baseIndex].reputation.level:
          addButton(name = ".recipes", label = "Buy recipes",
              command = "ShowBaseUI recipes", shortcut = "y", underline = 2)
          break
      except:
        showError(message = "Can't check if base has recipes for sale.")
        return true
    if skyBases[baseIndex].missions.len > 0:
      var missionsLimit: int = case skyBases[baseIndex].reputation.level
        of 0..25:
          1
        of 26..50:
          3
        of 51..75:
          5
        of 76..100:
          10
        else:
          0
      for mission in acceptedMissions:
        if (mission.finished and mission.startBase == baseIndex) or (
            mission.targetX == playerShip.skyX and mission.targetY ==
            playerShip.skyY):
          case mission.mType
          of deliver:
            try:
              addButton(name = ".mission", label = "Complete delivery of " &
                  itemsList[mission.itemIndex].name,
                  command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
            except:
              showError(message = "Can't add mission button.")
              return true
          of destroy:
            if mission.finished:
              try:
                addButton(name = ".mission", label = "Complete destroy " &
                    protoShipsList[mission.shipIndex].name,
                    command = "CompleteMission", shortcut = "c",
                        underline = 0, row = 0)
              except:
                showError(message = "Can't add mission button.")
                return true
          of patrol:
            if mission.finished:
              addButton(name = ".mission",
                  label = "Complete Patrol area mission",
                  command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
          of explore:
            if mission.finished:
              addButton(name = ".mission",
                  label = "Complete Explore area mission",
                  command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
          of passenger:
            if mission.finished:
              addButton(name = ".mission",
                  label = "Complete Transport passenger mission",
                  command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
        if mission.startBase == baseIndex:
          missionsLimit.dec
      if missionsLimit > 0:
        addButton(name = ".missions", label = "Missions",
            command = "ShowBaseMissions", shortcut = "m", underline = 0)
    if playerShip.homeBase != baseIndex:
      addButton(name = ".home", label = "Set as home", command = "SetAsHome",
          shortcut = "h", underline = 7)
  else:
    addButton(name = ".loot", label = "Loot", command = "ShowLoot",
        shortcut = "l", underline = 0)

proc showOrdersCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show available the player's ship's orders to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowOrders
  var ordersMenu: string = createDialog(name = ".gameframe.orders",
      title = "Ship orders")
  if tclEval2(script = "winfo ismapped " & ordersMenu) == "1":
    return closeDialogCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  if tclGetVar(varName = "gamestate") == "combat":
    tclEval(script = "busy forget " & mainPaned)
    tclEval(script = "busy forget " & gameHeader)
    tclEval(script = "destroy " & ordersMenu)
    return tclOk
  var haveTrader: bool = false
  if findMember(order = talk) > -1:
    haveTrader = true
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    dialogCloseButton: string = ordersMenu & ".closebutton"
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -text Close -command {CloseDialog " & ordersMenu & "}")
  lastButton = "."
  shortcuts = @[]

  if currentStory.index.len > 0:
    let step: StepData = try:
        (if currentStory.currentStep == -1: storiesList[
          currentStory.index].startingStep elif currentStory.currentStep >
          -1: storiesList[currentStory.index].steps[
          currentStory.currentStep] else: storiesList[
          currentStory.index].finalStep)
      except:
        return showError(message = "Can't get the current story step.")
    case step.finishCondition
    of askInBase:
      if baseIndex > 0:
        if currentStory.data.len == 0 or currentStory.data == skyBases[
            baseIndex].name:
          try:
            addButton(name = ".story", label = "Ask for " & itemsList[
                getStepData(finishData = step.finishData,
                    name = "item").parseInt].name,
                command = "ExecuteStory", shortcut = "f", underLine = 4)
          except:
            return showError(message = "Can't add the story button.")
    of destroyShip:
      let parts: seq[string] = currentStory.data.split(sep = ';')
      try:
        if playerShip.skyX == parts[0].parseInt and playerShip.skyY == parts[1].parseInt:
          try:
            addButton(name = ".story", label = "Search for " & protoShipsList[
                parts[3].parseInt].name, command = "ExecuteStory",
                    shortcut = "s", underline = 0)
          except:
            return showError(message = "Can't add the story button.")
      except:
        return showError(message = "Can't get the story step location.")
    of explore:
      let parts: seq[string] = currentStory.data.split(sep = ';')
      try:
        if playerShip.skyX == parts[0].parseInt and playerShip.skyY == parts[1].parseInt:
          addButton(name = ".story", label = "Search area",
              command = "ExecuteStory", shortcut = "s", underline = 0)
      except:
        return showError(message = "Can't get the story step location.")
    of any, loot:
      discard
  if playerShip.speed == docked:
    if showDockedCommands(baseIndex = baseIndex, haveTrader = haveTrader):
      return tclOk
  else:
    var event: EventsTypes = EventsTypes.none
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
      event = eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].eType
    case event
    of enemyShip, enemyPatrol:
      addButton(name = ".event", label = "Attack", command = "Attack",
          shortcut = "a", underline = 0)
    of fullDocks:
      addButton(name = ".event", label = "Wait (full docks)",
          command = "ShowWait", shortcut = "w", underline = 0)
    of attackOnBase:
      addButton(name = ".event", label = "Defend", command = "Attack",
          shortcut = "d", underline = 0)
    of disease:
      if haveTrader:
        let itemIndex: int = try:
            findItem(inventory = playerShip.cargo,
              itemType = factionsList[skyBases[baseIndex].owner].healingTools,
              itemQuality = any)
          except:
            return showError(message = "Can't find medicinet in the ship cargo.")
        if itemIndex > -1:
          addButton(name = ".deliverfree", label = "Deliver medicines for free",
              command = "DeliverMedicines free", shortcut = "d", underline = 0)
          addButton(name = ".deliverprice",
              label = "Deliver medicines for price",
              command = "DeliverMedicines paid", shortcut = "m", underline = 8)
    of EventsTypes.none, doublePrice, baseRecovery:
      if baseIndex > 0:
        if skyBases[baseIndex].reputation.level > -25:
          var dockingCost: int = 1
          for module in playerShip.modules:
            if module.mType == ModuleType2.hull:
              dockingCost = module.maxModules
              break
          if getBasePopulation(baseIndex = baseIndex) > empty:
            addButton(name = ".dock", label = "Dock (" & $dockingCost & " " &
                moneyName & ")", command = "Docking", shortcut = "d", underline = 0)
          else:
            addButton(name = ".dock", label = "Dock", command = "Docking",
                shortcut = "d", underline = 0)
        for mission in acceptedMissions:
          if haveTrader and mission.targetX == playerShip.skyX and
              mission.targetY == playerShip.skyY and mission.finished:
            case mission.mType
            of deliver:
              try:
                addButton(name = ".mission", label = "Complete delivery of " &
                    itemsList[mission.itemIndex].name,
                    command = "CompleteMission", shortcut = "c", underline = 0)
              except:
                return showError(message = "Can't add accepted mission button.")
            of destroy:
              try:
                addButton(name = ".mission", label = "Complete destroy " &
                    protoShipsList[mission.shipIndex].name,
                    command = "CompleteMission", shortcut = "c", underline = 0)
              except:
                return showError(message = "Can't add accepted mission button.")
            of patrol:
              addButton(name = ".mission",
                  label = "Complete Patrol area mission",
                  command = "CompleteMission", shortcut = "c", underline = 0)
            of explore:
              addButton(name = ".mission",
                  label = "Complete Explore area mission",
                  command = "CompleteMission", shortcut = "c", underline = 0)
            of passenger:
              addButton(name = ".mission",
                  label = "Complete Transport passenger mission",
                  command = "CompleteMission", shortcut = "c", underline = 0)
      else:
        for mission in acceptedMissions:
          if mission.targetX == playerShip.skyX and mission.targetY ==
              playerShip.skyY and not mission.finished:
            case mission.mType
            of deliver, passenger:
              discard
            of destroy:
              try:
                addButton(name = ".mission", label = "Search for " &
                    protoShipsList[mission.shipIndex].name,
                    command = "StartMission", shortcut = "s", underline = 0)
              except:
                return showError(message = "Can't add accepted mission button.")
            of patrol:
              addButton(name = ".mission", label = "Patrol area",
                  command = "StartMission", shortcut = "p", underline = 0)
            of explore:
              addButton(name = ".mission", label = "Explore area",
                  command = "StartMission", shortcut = "e", underline = 0)
    of trader:
      if haveTrader:
        addButton(name = ".trade", label = "Trade", command = "ShowTrader " &
            $eventsList[skyMap[playerShip.skyX][
            playerShip.skyY].eventIndex].shipIndex, shortcut = "t", underline = 0)
        addButton(name = ".askevents", label = "Ask for events",
            command = "AskForEvents", shortcut = "e", underline = 8)
        addButton(name = ".askbases", label = "Ask for bases",
            command = "AskForBases", shortcut = "b", underline = 8)
      addButton(name = ".attack", label = "Attack", command = "Attack",
          shortcut = "a", underline = 0)
    of friendlyShip:
      if haveTrader:
        try:
          if tradersName in protoShipsList[eventsList[skyMap[playerShip.skyX][
              playerShip.skyY].eventIndex].shipIndex].name:
            addButton(name = ".trade", label = "Trade",
                command = "ShowTrader " & $eventsList[skyMap[playerShip.skyX][
                playerShip.skyY].eventIndex].shipIndex, shortcut = "t", underline = 0)
            addButton(name = ".askbases", label = "Ask for bases",
                command = "AskForBases", shortcut = "b", underline = 8)
        except:
          return showError(message = "Can't check if ship is trader.")
        addButton(name = ".askevents", label = "Ask for events",
            command = "AskForEvents", shortcut = "e", underline = 8)
      addButton(name = ".attack", label = "Attack", command = "Attack",
          shortcut = "a", underline = 0)
  if lastButton == ".":
    showMessage(text = "Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.",
        title = "No orders available")
  else:
    tclEval(script = "grid " & dialogCloseButton & " -sticky we -padx 5 -pady {0 5}")
    tclEval(script = "bind " & dialogCloseButton & " <Escape> {" &
        dialogCloseButton & " invoke;break}")
    tclEval(script = "bind " & lastButton & " <Tab> {focus " &
        dialogCloseButton & ";break}")
    for shortcut in shortcuts:
      tclEval(script = "bind " & dialogCloseButton & " <Alt-" &
          shortcut.shortcut & "> {" & shortcut.buttonName & " invoke;break}")
    for button in shortcuts:
      let menuButton: string = button.buttonName
      for shortcut in shortcuts:
        tclEval(script = "bind " & menuButton & " <Alt-" & shortcut.shortcut &
            "> {" & shortcut.buttonName & " invoke;break}")
    showDialog(dialog = ordersMenu, parentFrame = ".gameframe", relativeX = 0.4,
        relativeY = (if playerShip.speed == docked: 0.1 else: 0.3))
    tclEval(script = "focus " & dialogCloseButton)
  return tclOk

proc dockingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Dock or undock from the sky base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Docking ?escape?
  ## If argument escape is present, escape from the base without paying,
  ## otherwise normal docking or undocking operation

proc askForBasesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Ask for bases in the currently docked base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## AskForBases

proc askForEventsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Ask for events in the currently docked base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## AskForEvents

proc attackCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], cdecl, contractual, ruleOff: "params".} =
  ## Start the combat between ships
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Attack
  showCombatUi()
  return tclOk

proc prayCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Pray in the selected base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Pray

proc setAsHomeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Set the selected base as the home base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetAsHome
  let traderIndex: int = findMember(order = talk)
  var price: Natural = 1_000
  try:
    countPrice(price = price, traderIndex = traderIndex)
  except:
    return showError(message = "Can't count the price for set as home.")
  showQuestion(question = "Are you sure want to change your home base (it cost " &
      $price & " " & moneyName & ")?", res = "sethomebase")
  return tclOk

proc showTraderCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Generate cargo for the trader and show the trading UI
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowTrader protoindex
  ## Protoindex is the index of ship prototype on which trader cargo will be
  try:
    generateTraderCargo(protoIndex = ($argv[1]).parseInt)
  except:
    return showError(message = "Can't generated the trader's cargo.")
  tclEval(script = "ShowTrade");
  return tclOk

proc startMissionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Start the selected mission
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## StartMission

proc completeMissionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Complete the selected mission in the base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## CompleteMission

proc executeStoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Execute the current step in the current story
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ExecuteStory

proc deliverMedicinesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.}
  ## Deliver medicines to the base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DeliverMedicines type
  ## If argument type is free, deliver medicines for free, otherwise deliver
  ## medicines for a price

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the orders menu
  try:
    addCommand(name = "ShowOrders", nimProc = showOrdersCommand)
    addCommand(name = "Docking", nimProc = dockingCommand)
    addCommand(name = "AskForBases", nimProc = askForBasesCommand)
    addCommand(name = "AskForEvents", nimProc = askForEventsCommand)
    addCommand(name = "Attack", nimProc = attackCommand)
    addCommand(name = "Pray", nimProc = prayCommand)
    addCommand(name = "SetAsHome", nimProc = setAsHomeCommand)
    addCommand(name = "ShowTrader", nimProc = showTraderCommand)
    addCommand(name = "StartMission", nimProc = startMissionCommand)
    addCommand(name = "CompleteMission", nimProc = completeMissionCommand)
    addCommand(name = "ExecuteStory", nimProc = executeStoryCommand)
    addCommand(name = "DeliverMedicines", nimProc = deliverMedicinesCommand)
  except:
    showError(message = "Can't add a Tcl command.")

import mapsui, waitmenu

{.push ruleOff: "hasPragma".}
{.push ruleOff: "hasDoc".}
proc dockingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  var message: string = ""
  if playerShip.speed == docked:
    try:
      message = (if argc == 1: dockShip(docking = false) else: dockShip(
          docking = false, escape = true))
    except:
      return showError(message = "Can't undock from the base.")
    if message.len > 0:
      showMessage(text = message, title = "Can't undock from base")
      return tclOk
  else:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
      if eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].eType == fullDocks:
        return showWaitCommand(clientData = clientData, interp = interp,
            argc = argc, argv = argv)
    try:
      message = dockShip(docking = true)
    except:
      return showError(message = "Can't dock to the base.")
    if message.len > 0:
      showMessage(text = message, title = "Can't dock to base")
      return tclOk
  showSkyMap(clear = true)
  if playerShip.speed == docked:
    return showOrdersCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  return tclOk

proc askForBasesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  try:
    askForBases()
  except:
    return showError(message = "Can't ask for bases.")
  showSkyMap()
  return tclOk

proc askForEventsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  try:
    askForEvents()
  except:
    return showError(message = "Can't ask for events.")
  showSkyMap()
  return tclOk

proc prayCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  for index, _ in playerShip.crew:
    try:
      updateMorale(ship = playerShip, memberIndex = index, value = 10)
    except:
      return showError(message = "Can't update morale of crew member.")
  addMessage(message = "You and your crew were praying for some time. Now you all feel a bit better.",
      mType = orderMessage)
  try:
    updateGame(minutes = 30)
  except:
    return showError(message = "Can't update the game.")
  showSkyMap()
  return tclOk

proc startMissionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  var startsCombat, uMission: bool = false
  for mission in acceptedMissions:
    if mission.targetX == playerShip.skyX and mission.targetY ==
        playerShip.skyY and not mission.finished:
      case mission.mType
      of deliver, passenger:
        discard
      of destroy:
        try:
          updateGame(minutes = getRandom(min = 15, max = 45))
          startsCombat = checkForEvent()
          if not startsCombat:
            startsCombat = startCombat(enemyIndex = mission.shipIndex,
                newCombat = false)
        except:
          return showError(message = "Can't start destroy mission.")
      of patrol:
        try:
          updateGame(minutes = getRandom(min = 45, max = 75))
          startsCombat = checkForEvent()
        except:
          return showError(message = "Can't start patrol mission.")
        if not startsCombat:
          uMission = true
      of explore:
        try:
          updateGame(minutes = getRandom(min = 30, max = 60))
          startsCombat = checkForEvent()
        except:
          return showError(message = "Can't start explore mission.")
        if not startsCombat:
          uMission = true
  if startsCombat:
    showCombatUi()
    return tclOk
  if uMission:
    try:
      updateMission(missionIndex = skyMap[playerShip.skyX][
          playerShip.skyY].missionIndex)
    except:
      return showError(message = "Can't update the mission.")
  updateHeader()
  updateMessages()
  showSkyMap()
  return tclOk

proc completeMissionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  try:
    finishMission(missionIndex = skyMap[playerShip.skyX][
        playerShip.skyY].missionIndex)
  except MissionFinishingError:
    showInfo(text = getCurrentExceptionMsg(),
        title = "Can't finish the mission")
    return tclOk
  except:
    return showError(message = "Can't finish the mission.")
  updateHeader()
  updateMessages()
  showSkyMap()
  return tclOk

proc executeStoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  var step: StepData = try:
        (if currentStory.currentStep == -1: storiesList[
        currentStory.index].startingStep elif currentStory.currentStep >
        -1: storiesList[currentStory.index].steps[
        currentStory.currentStep] else: storiesList[
            currentStory.index].finalStep)
      except:
        return showError(message = "Can't get the current story step.")
  if playerShip.speed != docked and step.finishCondition == askInBase:
    let message: string = try:
        dockShip(docking = true)
      except:
        return showError(message = "Can't dock to the base.")
    if message.len > 0:
      showInfo(text = message, title = "Can't dock to base")
      return tclOk
  try:
    if progressStory():
      let tokens: seq[string] = currentStory.data.split(sep = ';')
      case step.finishCondition
      of destroyShip:
        if startCombat(enemyIndex = tokens[2].parseInt, newCombat = false):
          showCombatUi()
          return tclOk
      else:
        discard
      if currentStory.currentStep > -3:
        step = (if currentStory.currentStep > -1: storiesList[
            currentStory.index].steps[currentStory.currentStep] else: storiesList[
            currentStory.index].finalStep)
        for text in step.texts:
          if currentStory.finishedStep == text.condition:
            showInfo(text = text.text, title = "Story")
            break
      else:
        finishStory()
    else:
      showInfo(text = step.failText, title = "Story")
      currentStory.showText = false
  except:
    return showError(message = "Can't progress the current story.")
  updateHeader()
  updateMessages()
  showSkyMap()
  return tclOk

proc deliverMedicinesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    eventIndex: int = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
    itemIndex: int = try:
        findItem(inventory = playerShip.cargo, itemType = factionsList[skyBases[
            baseIndex].owner].healingTools, itemQuality = any)
      except:
        return showError(message = "Can't get index of medicines.")
    event: EventData = eventsList[eventIndex]
    newTime: int = event.time - playerShip.cargo[itemIndex].amount
  if newTime < 1:
    deleteEvent(eventIndex = eventIndex)
  if argv[1] == "free":
    try:
      gainRep(baseIndex = baseIndex, points = (playerShip.cargo[
          itemIndex].amount / 10).Natural)
    except:
      return showError(message = "Can't gain reputation in base.")
    try:
      addMessage(message = "You gave " & itemsList[playerShip.cargo[
          itemIndex].protoIndex].name & " for free to base.",
          mType = tradeMessage)
    except:
      return showError(message = "Can't show message.")
    try:
      updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
          itemIndex].protoIndex, amount = -(playerShip.cargo[itemIndex].amount),
          quality = playerShip.cargo[itemIndex].quality)
    except:
      return showError(message = "Can't update the ship' cargo.")
  else:
    try:
      gainRep(baseIndex = baseIndex, points = (playerShip.cargo[
          itemIndex].amount / 20).int * (-1))
    except:
      return showError(message = "Can't gain reputation in base.")
    try:
      sellItems(itemIndex = itemIndex, amount = $playerShip.cargo[
          itemIndex].amount)
    except TradeNoFreeCargoError:
      showMessage(text = "You can't sell medicines to the base because you don't have enough free cargo space for money.",
          title = "No free cargo space")
    except NoMoneyInBaseError:
      showMessage(text = "You can't sell medicines to the base because the base don't have enough money to buy them.",
          title = "Can't sell medicines")
    except:
      return showError(message = "Can't sell medicines to base.")
  updateHeader()
  updateMessages()
  showSkyMap()
  return tclOk
