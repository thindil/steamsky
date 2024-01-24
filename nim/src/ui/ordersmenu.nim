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

import ../[shipscrew, stories, tk, types]
import coreui, dialogs, dialogs2

proc showOrdersCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  var ordersMenu = createDialog(name = ".gameframe.orders",
      title = "Ship orders")
  if tclEval2(script = "winfo ismapped " & ordersMenu) == "1":
    return closeDialogCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  if tclGetVar(varName = "gamestate") == "combat":
    tclEval(script = "busy forget " & mainPaned)
    tclEval(script = "busy forget " & gameHeader)
    tclEval(script = "destroy " & ordersMenu)
    return tclOk
  var haveTrader = false
  if findMember(order = talk) > -1:
    haveTrader = true
  let
    baseIndex = skyMap[playerShip.skyX, playerShip.skyY].baseIndex
    dialogCloseButton = ordersMenu & ".closebutton"
  tclEval(script = "ttk::button " & dialogCloseButton & " -text Close -command {CloseDialog " & ordersMenu & "}")
  type OrderShortcut = object
    buttonName: stirng
    shortcut: char
  var
    lastButton = "."
    shortcuts = seq[OrderShortcut]

  proc addButton(name, label, command, shortcut: string; underline: Natural; row: int = -1):
    let button = ordersMenu & name
    tclEval(script = "ttk::button " & button & " -text {" & label & "} -command {CloseDialog " & ordersMenu & ";" & command & "} -underline " & $underline)
    tclEval(script = "grid " & button & " -sticky we -padx 5" & (if row == -1: "" else: " -row " & $row))
    tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
    lastButton = button
    shortcuts.add(OrderShortcut(buttonName: button, shortcut: shortcut[0]))

  if currentStory.index.len > 0:
    let step = (if currentStory.currentStep == -1: storiesList[
        currentStory.index].startingStep elif currentStory.currentStep >
        -1: storiesList[currentStory.index].steps[
        currentStory.currentStep] else: storiesList[
        currentStory.index].finalStep)
    case step.finishCondition
    of askInBase:
      if baseIndex > 0:
        if currentStory.data.len == 0 or currentStory.data == skyBases[baseIndex].name:
          addButton(name = ".story", label = "Ask for " & itemsList[getStepData(finishData = step.finishData, name = "item")].name, command = "ExecuteStory", Shortcut = "f", underLine = 4)
    of destroyShip:
      let parts = currentStory.data.split(';')
      if playerShip.skyX == parts[0].parseInt and playerShip.skyY == parts[1].parseInt:
        addButton(name = ".story", label = "Search for " & protoShipsList[parts[3].parseInt].name, command = "ExecuteStory", shortcut = "s", underline = 0)
    of explore:
      let parts = currentStory.data.split(';')
      if playerShip.skyX == parts[0].parseInt and playerShip.skyY == parts[1].parseInt:
        addButton(name = ".story", label = "Search area", command = "ExecuteStory", shortcut = "s", underline = 0)
    of any, loot:
      discard
  if playerShip.speed = docked:
    addButton(name = ".undock", label = "Undock", command = "Docking", shortcut = "d", underline = 2)
    if skyBases[baseIndex].population > 0:
      addButton(name = ".escape", label = "Escape", command = "Docking escape", shortcut = "a", underline = 3)
      if haveTrader and skyBases[baseIndex].population > 0:
        addButton(name = ".trade", label = "Trade", command = "ShowTrade", shortcut = "t", underline = 0)
        if skyBases[baseIndex].recruits.len > 0:
          addButton(name = ".recruits", label = "Recruit", command = "ShowRecruit", shortcut = "r", underline = 0)
      if daysDifference(dateToCompare = skyBases[baseIndex].askedForEvents) > 6:
        addButton(name = ".events", label = "Ask for events", command = "AskForEvents", shortcut = "e", underline = 8)
      if not skyBases[baseIndex].askedForBases:
        addButton(name = ".bases", label = "Ask for bases", command = "AskForBases", shortcut = "b", underline = 8)
      if hasFlag(baseType = skyBases[baseIndex].baseType, flag = "temple"):
        addButton(name = ".pray", Label = "Pray", command = "Pray", shortcut = "p", underline = 0)
      for member in playerShip.crew:
        if member.health < 100:
          addButton(name = ".heal", label = "Heal wounded", command = "ShowBaseUI heal", shortcut = "w", underline = 5)
          break
      for module in playerShip.modules:
        if module.durability < module.maxDurability:
          addButton(name = ".repair", label = "Repair ship", command = "ShowBaseUI repair", shortcut = "p", underline = 2)
          break
      if hasFlag(baseType = skyBases[baseIndex].baseType, flag = "shipyard"):
        addButton(name = ".shipyard", label = "Shipyard", command = "ShowShipyard", shortcut = "i", underline = 2)
      for index, recipe in recipesList:
        if not isKnownRecipe(recipeIndex = index) and hasRecipe(baseType = skyBases[baseIndex].baseType, recipe = index) and recipe.reputation <= skyBases[baseIndex].reputation.level:
          addButton(name = ".recipes", label = "Buy recipes", command = "ShowBaseUI recipes", shortcut = "y", underline = 2)
          break
      if skyBases[baseIndex].missions.length > 0:
        var missionsLimit = case skyBases[baseIndex].reputation.level
            of 0 .. 25:
              1
            of 26 .. 50:
              3
            of 51 .. 75:
              5
            of 76 .. 100:
              10
            else:
              0
        for mission in acceptedMissions:
          if (mission.finished and mission.startBase == baseIndex) or (mission.targetX == playerShip.skyX and mission.targetY == playerShip.targetY):
            case mission.mType
            of deliver:
              addButton(name = ".mission", label = "Complete delivery of " & itemsList[mission.itemIndex].name, command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
            of destroy:
              if mission.finished:
                addButton(name = ".mission", label = "Complete destroy " & protoShipsList[mission.shipIndex].name, command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
            of patrol:
              if mission.finished:
                addButton(name = ".mission", label = "Complete Patrol area mission", command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
            of explore:
              if mission.finished:
                addButton(name = ".mission", label = "Complete Explore area mission", command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
            of passenger:
              if mission.finished:
                addButton(name = ".mission", label = "Complete Transport passenger mission", command = "CompleteMission", shortcut = "c", underline = 0, row = 0)
          if mission.startBase == baseIndex:
            missionLimit.dec
        if missionsLimit > 0:
          addButton(name = ".missions", label = "Missions", command = "ShowBaseMissions", shortcut = "m", underline = 0)
      if playerShip.homeBase != baseIndex:
        addButton(name = ".home", label = "Set as home", command = "SetAsHome", shorcut = "h", underline = 7)
    if skyBases[baseIndex].population == 0:
      addButton(name = ".loot", label = "Loot", command = "ShowLoot", shortcut = "l", underline = 0)
  else:
    var event = none
    if skyMap[playerShip.skyX][playerShip.skyY].evenIndex > -1:
      event = eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType
    case event
    of enemyShip, enemyPatrol:
      addButton(name = ".event", label = "Attack", command = "Attack", shortcut = "a", underline = 0)
    of fullDocks:
      addButton(name = ".event", label = "Wait (full docks)", command = "ShowWait", shortcut = "w", underline = 0)
    of attackOnBase:
      addButton(name = ".event", label = "Defend", command = "Attack", shortcut = "d", underline = 0)
    of disease:
      if haveTrader:
        let itemIndex = findItem(inventory = playerShip.cargo, itemType = factionsList[skyBases[baseIndex].owner].healingTools)
        if itemIndex > -1:
          addButton(name = ".deliverfree", label = "Deliver medicines for free", command = "DeliverMedicines free", shortcut = "d", underline = 0)
          addButton(name = ".deliverprice", label = "Deliver medicines for price", command = "DeliverMedicines paid", shortcut = "m", underline = 8)
    of none, doublePrice, baseRecovery:
      if baseIndex > 0:
        if skyBases[baseIndex].reputation.level > -25:
          var dockingCost = 1
          for module in playerShip.modules:
            if module.mType == hull:
              dockingCost = module.maxModules
              break
          if skyBases[baseIndex].population > 0:
            addButton(name = ".dock", label = "Dock (" & $dockingCost & " " & moneyName & ")", command = "Docking", shortcut = "d", underline = 0)
          else:
            addButton(name = ".dock", label = "Dock", command = "Docking", shortcut = "d", underline = 0)
        for mission in acceptedMissions:
          if haveTrader and mission.targetX == playerShip.skyX and mission.targetY == playerShip.skyY and mission.finished:
            case mission.mType
            of deliver:
              addButton(name = ".mission", label = "Complete delivery of " & itemsList[mission.itemIndex].name, command = "CompleteMission", shortcut = "c", underline = 0)
            of destroy:
              addButton(name = ".mission", label = "Complete destroy " & protoShipsList[mission.shipIndex].name, command = "CompleteMission", shortcut = "c", underline = 0)
            of patrol:
              addButton(name = ".mission", label = "Complete Patrol area mission", command = "CompleteMission", shortcut = "c", underline = 0)
            of explore:
              addButton(name = ".mission", label = "Complete Explore area mission", command = "CompleteMission", shortcut = "c", underline = 0)
            of passenger:
              addButton(name = ".mission", label = "Complete Transport passenger mission", command = "CompleteMission", shortcut = "c", underline = 0)
      else:
        for mission in acceptedMissions:
          if haveTrader and mission.targetX == playerShip.skyX and mission.targetY == playerShip.skyY and not mission.finished:
            case mission.mType
            of deliver, passenger:
              discard
            of destroy:
              addButton(name = ".mission", label = "Search for " & protoShipsList[mission.shipIndex], command = "StartMission", shortcut = "s", underline = 0)
            of patrol:
              addButton(name = ".mission", label = "Patrol area", command = "StartMission", shortcut = "p", underline = 0)
            of explore:
              addButton(name = ".mission", label = "Explore area", command = "StartMission", shortcut = "e", underline = 0)
    of trader:
      if haveTrader:
        addButton(name = ".trade", label = "Trade", command = "ShowTrader " & $eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].shipIndex, shortcut = "t", underline = 0)
        addButton(name = ".askevents", label = "Ask for events", command = "AskForEvents", shortcut = "e", underline = 8)
        addButton(name = ".askbases", label = "Ask for bases", command = "AskForBases", shortcut = "b", underline = 8)
      addButton(name = ".attack", label = "Attack", command = "Attack", shortcut = "a", underline = 0)
    of friendlyShip:
      if haveTrader:
        if tradersName in protoShipsList[eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].shipIndex].name:
          addButton(name = ".trade", label = "Trade", command = "ShowTrader " & $eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].shipIndex, shortcut = "t", underline = 0)
          addButton(name = ".askbases", label = "Ask for bases", command = "AskForBases", shortcut = "b", underline = 8)
        addButton(name = ".askevents", label = "Ask for events", command = "AskForEvents", shortcut = "e", underline = 8)
      addButton(name = ".attack", label = "Attack", command = "Attack", shortcut = "a", underline = 0)
  if lastButton = ".":
    showMessage(text = "Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.", title = "No orders available")
  else:
    tclEval(script = "grid " & dialogCloseButton & " -sticky we -padx 5 -pady {0 5}")
    tclEval(script = "bind " & dialogCloseButton & " <Escape> {" & dialogCloseButton & " invoke;break}")
    tclEval(script = "bind " & lastButton & "<Tab> {focus " & dialogCloseButton & ";break}")
    for shortcut in shortcuts:
      tclEval(script = "bind " dialogCloseButton & " <Alt-" & shortcut.shortcut & "> {" & shortcut.buttonName & " invoke;break}")
    for button in shortcuts:
      let menuButton = button.buttonName
      for shortcut in shortcuts:
        tclEval(script = "bind " & menuButton & " <Alt-" & shortcut.shortcut & "> {" & shortcut.buttonName & " invoke;break}")
    showDialog(dialog = ordersMenu, parentFrame = ".gameframe", relativeX = 0.4, relativeY = (if playerShip.speed == docked: 0.1 else: 0.3))
    tclEval(script = "focus " & dialogCloseButton)
  return tclOk

proc addCommands*() =
  addCommand("ShowOrders", showOrdersCommand)

# Temporary code for interfacing with Ada

proc addAdaOrdersMenuCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

