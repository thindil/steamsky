# Copyright 2025 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the player's ship's orders menu, like counting
## the menu's height, showing the menu and executing some of its commands.

import std/[tables, strutils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, bases2, basestypes, combat, crewinventory, events, events2,
    game, game2, maps, messages, missions, missions2, shipscargo, shipscrew,
    shipsmovement, statistics, stories, stories2, trades, types, utils]
import coreui, dialogs, errordialog, setui, waitmenu

proc countHeight(baseIndex: ExtendedBasesRange;
    haveTrader: bool; dialog: var GameDialog): Positive {.raises: [], tags: [
        RootEffect], contractual.} =
  ## Count the height of the orders menu, based on the amount of buttons inside
  ##
  ## * baseIndex  - the index of the base to which the player's ship is docked
  ## * haveTrader - if true, someone in the crew is assigned to trader position
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened.
  result = 75
  if playerShip.speed == docked:
    result += 35
    if getBasePopulation(baseIndex = baseIndex) == empty:
      result += 35
    else:
      result += 35
      if haveTrader:
        result += 70
        if skyBases[baseIndex].recruits.len > 0:
          result += 35
      if daysDifference(dateToCompare = skyBases[baseIndex].askedForEvents) > 6:
        result += 35
      if not skyBases[baseIndex].askedForBases:
        result += 35
      try:
        if "temple" in basesTypesList[skyBases[baseIndex].baseType].flags:
          result += 35
      except:
        dialog = setError(message = "Can't check if base has temple flag.")
        return
      for member in playerShip.crew:
        if member.health < 100:
          result += 35
          break
      for module in playerShip.modules:
        if module.durability < module.maxDurability:
          result += 35
          break
      try:
        if "shipyard" in basesTypesList[skyBases[baseIndex].baseType].flags:
          result += 35
      except:
        dialog = setError(message = "Can't check if the base has shipyard flag.")
        return
      for index, recipe in recipesList:
        try:
          if index notin knownRecipes and index in basesTypesList[skyBases[
              baseIndex].baseType].recipes and recipe.reputation <= skyBases[
              baseIndex].reputation.level:
            result += 35
            break
        except:
          dialog = setError(message = "Can't check if base has recipes for sale.")
          return
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
            if mission.mType == deliver or mission.finished:
              result += 35
          if mission.startBase == baseIndex:
            missionsLimit.dec
        if missionsLimit > 0:
          result += 35
      if playerShip.homeBase != baseIndex:
        result += 35
  else:
    result += 5
    var event: EventsTypes = EventsTypes.none
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
      event = eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].eType
    case event
    of enemyShip, enemyPatrol:
      result += 35
    of fullDocks:
      result += 35
    of attackOnBase:
      result += 35
    of disease:
      if haveTrader:
        let itemIndex: int = try:
            findItem(inventory = playerShip.cargo,
              itemType = factionsList[skyBases[baseIndex].owner].healingTools, itemQuality = any)
          except:
            dialog = setError(message = "Can't find medicine in the ship cargo.")
            return
        if itemIndex > -1:
          result += 70
    of EventsTypes.none, doublePrice, baseRecovery:
      if baseIndex > 0:
        if skyBases[baseIndex].reputation.level > -25:
          result += 35
        for mission in acceptedMissions:
          if haveTrader and mission.targetX == playerShip.skyX and
              mission.targetY == playerShip.skyY and mission.finished:
            result += 35
      else:
        for mission in acceptedMissions:
          if mission.targetX == playerShip.skyX and mission.targetY ==
              playerShip.skyY and not mission.finished:
            if mission.mType notin {deliver, passenger}:
              result += 35
    of trader:
      if haveTrader:
        result += 105
      result += 35
    of friendlyShip:
      if haveTrader:
        try:
          if tradersName in protoShipsList[eventsList[skyMap[playerShip.skyX][
              playerShip.skyY].eventIndex].shipIndex].name:
            result += 70
        except:
          dialog = setError(message = "Can't check if ship is trader.")
          return
        result += 35
      result += 35

proc dockingOrder(escape: bool = false; dialog: var GameDialog;
    state: var GameState) {.raises: [], tags: [RootEffect], contractual.} =
  ## Docking, undocking and escaping from bases
  ##
  ## * escape  - if true, escape from a base
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current state of the game
  ##
  ## Returns the modified parameters dialog if error happened. Additionally,
  ## modified parameter state when the player undock from a base.
  var message: string = ""
  if playerShip.speed == docked:
    try:
      message = dockShip(docking = false, escape = escape)
    except:
      dialog = setError(message = "Can't undock from the base.")
      return
    if message.len > 0:
      dialog = setMessage(message = message, title = "Can't undock from base")
      return
    state = map
  else:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
      if eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].eType == fullDocks:
        return
    try:
      message = dockShip(docking = true)
    except:
      dialog = setError(message = "Can't dock to the base.")
      return
    if message.len > 0:
      dialog = setMessage(message = message, title = "Can't dock to base")
      return
  dialog = none
  closePopup()

proc completeMission(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Complete the current mission
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened.
  closePopup()
  try:
    finishMission(missionIndex = skyMap[playerShip.skyX][
        playerShip.skyY].missionIndex)
  except MissionFinishingError:
    dialog = setInfo(text = getCurrentExceptionMsg(),
        title = "Can't finish the mission")
  except:
    dialog = setError(message = "Can't finish the mission.")

proc setAsHome(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the current base as the home base
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog.
  closePopup()
  let traderIndex: int = findMember(order = talk)
  var price: Natural = 1_000
  try:
    countPrice(price = price, traderIndex = traderIndex)
  except:
    dialog = setError(message = "Can't count the price for set as home.")
    return
  dialog = setQuestion(question = "Are you sure want to change your home base (it cost " &
      $price & " " & moneyName & ")?", qType = homeBase, data = $price)

proc askForEvents(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Ask for known events
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog.
  try:
    askForEvents()
    dialog = none
    closePopup()
  except:
    dialog = setError(message = "Can't ask for events.")

proc askForBases(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Ask for known bases
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog.
  try:
    askForBases()
    dialog = none
    closePopup()
  except:
    dialog = setError(message = "Can't ask for bases.")

proc showDockedCommands(baseIndex: ExtendedBasesRange; haveTrader: bool;
    dialog: var GameDialog; state: var GameState) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the available orders when the player's ship is docked to a base
  ##
  ## * baseIndex  - the index of the base to which the player's ship is docked
  ## * haveTrader - if true, someone in the crew is assigned to trader position
  ## * dialog     - the current in-game dialog displayed on the screen
  ## * state      - the current state of the game
  ##
  ## Returns the modified parameters dialog if error happened.
  labelButton(title = "Undock"):
    dockingOrder(dialog = dialog, state = state)
  if getBasePopulation(baseIndex = baseIndex) == empty:
    labelButton(title = "Loot"):
      state = loot
      dialog = none
      closePopup()
      setLoot(dialog = dialog)
  else:
    labelButton(title = "Escape"):
      dockingOrder(escape = true, dialog = dialog, state = state)
    if haveTrader:
      labelButton(title = "Trade"):
        state = trade
        dialog = none
        closePopup()
        setTrade(dialog = dialog)
      labelButton(title = "School"):
        state = school
        dialog = none
        closePopup()
        setSchool(dialog = dialog)
      if skyBases[baseIndex].recruits.len > 0:
        labelButton(title = "Recruit"):
          state = recruits
          dialog = none
          closePopup()
          setRecruits(dialog = dialog)
    if daysDifference(dateToCompare = skyBases[baseIndex].askedForEvents) > 6:
      labelButton(title = "Ask for events"):
        askForEvents(dialog = dialog)
        if dialog != none:
          return
    if not skyBases[baseIndex].askedForBases:
      labelButton(title = "Ask for bases"):
        askForBases(dialog = dialog)
        if dialog != none:
          return
    try:
      if "temple" in basesTypesList[skyBases[baseIndex].baseType].flags:
        labelButton(title = "Pray"):
          for index, _ in playerShip.crew:
            try:
              updateMorale(ship = playerShip, memberIndex = index, value = 10)
            except:
              dialog = setError(message = "Can't update morale of crew member.")
              return
          addMessage(message = "You and your crew were praying for some time. Now you all feel a bit better.",
              mType = orderMessage)
          try:
            updateGame(minutes = 30)
            dialog = none
            closePopup()
          except:
            dialog = setError(message = "Can't update the game.")
            return
    except:
      dialog = setError(message = "Can't check if base has temple flag.")
      return
    for member in playerShip.crew:
      if member.health < 100:
        labelButton(title = "Heal wounded"):
          state = healWounded
          dialog = none
          closePopup()
          setWounded(dialog = dialog)
        break
    for module in playerShip.modules:
      if module.durability < module.maxDurability:
        labelButton(title = "Repair ship"):
          state = repairShip
          dialog = none
          closePopup()
          setRepairs(dialog = dialog)
        break
    try:
      if "shipyard" in basesTypesList[skyBases[baseIndex].baseType].flags:
        labelButton(title = "Shipyard"):
          state = shipyard
          dialog = none
          closePopup()
          setShipyard(dialog = dialog)
    except:
      dialog = setError(message = "Can't check if the base has shipyard flag.")
      return
    for index, recipe in recipesList:
      try:
        if index notin knownRecipes and index in basesTypesList[skyBases[
            baseIndex].baseType].recipes and recipe.reputation <= skyBases[
            baseIndex].reputation.level:
          labelButton(title = "Buy recipes"):
            state = buyRecipes
            dialog = none
            closePopup()
            setRecipes(dialog = dialog)
          break
      except:
        dialog = setError(message = "Can't check if base has recipes for sale.")
        return
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
              labelButton(title = "Complete delivery of " & itemsList[
                  mission.itemIndex].name):
                completeMission(dialog = dialog)
            except:
              dialog = setError(message = "Can't add mission button.")
              return
          of destroy:
            if mission.finished:
              try:
                labelButton(title = "Complete destroy " & protoShipsList[
                    mission.shipIndex].name):
                  completeMission(dialog = dialog)
              except:
                dialog = setError(message = "Can't add mission button.")
                return
          of patrol:
            if mission.finished:
              labelButton(title = "Complete Patrol area mission"):
                completeMission(dialog = dialog)
          of explore:
            if mission.finished:
              labelButton(title = "Complete Explore area mission"):
                completeMission(dialog = dialog)
          of passenger:
            if mission.finished:
              labelButton(title = "Complete Transport passenger mission"):
                completeMission(dialog = dialog)
        if mission.startBase == baseIndex:
          missionsLimit.dec
      if missionsLimit > 0:
        labelButton(title = "Missions"):
          state = baseMissions
          dialog = none
          closePopup()
          setMissions(dialog = dialog)
    if playerShip.homeBase != baseIndex:
      labelButton(title = "Set as home"):
        setAsHome(dialog = dialog)

proc finishStory(): GameDialog {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Finish the current player's story. Give experience and ask about
  ## finishing the game
  ##
  ## Returns current dialog of the game.
  gameStats.points += (10_000 * currentStory.maxSteps)
  clearCurrentStory()
  try:
    result = setQuestion(question = storiesList[currentStory.index].endText &
        " Do you want to finish the game?", qType = finishGame)
  except KeyError:
    result = setError(message = "Can't get the end text of the current story. ")

proc executeStory(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Execute the current story step
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current state of the game
  ##
  ## Returns the modified parameters dialog.
  closePopup()
  var step: StepData = try:
        (if currentStory.currentStep == -1: storiesList[
        currentStory.index].startingStep elif currentStory.currentStep >
        -1: storiesList[currentStory.index].steps[
        currentStory.currentStep] else: storiesList[
            currentStory.index].finalStep)
      except:
        dialog = setError(message = "Can't get the current story step.")
        return
  if playerShip.speed != docked and step.finishCondition == askInBase:
    let message: string = try:
        dockShip(docking = true)
      except:
        dialog = setError(message = "Can't dock to the base.")
        return
    if message.len > 0:
      dialog = setInfo(text = message, title = "Can't dock to base")
      return
  try:
    if progressStory():
      let tokens: seq[string] = currentStory.data.split(sep = ';')
      if step.finishCondition == destroyShip:
        if startCombat(enemyIndex = tokens[2].parseInt, newCombat = false):
          closePopup()
          setCombat(state = state, dialog = dialog)
          return
      if currentStory.currentStep > -3:
        step = (if currentStory.currentStep > -1: storiesList[
            currentStory.index].steps[currentStory.currentStep] else: storiesList[
            currentStory.index].finalStep)
        for text in step.texts:
          if currentStory.finishedStep == text.condition:
            dialog = setInfo(text = text.text, title = "Story")
            break
      else:
        dialog = finishStory()
    else:
      dialog = setInfo(text = step.failText, title = "Story")
      currentStory.showText = false
  except:
    dialog = setError(message = "Can't progress the current story.")


proc startMission(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Start the mission in the current map cell
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current game's state
  ##
  ## Returns the modified parameters dialog and state.
  var startsCombat, uMission: bool = false
  closePopup()
  dialog = none
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
          dialog = setError(message = "Can't start destroy mission.")
          return
      of patrol:
        try:
          updateGame(minutes = getRandom(min = 45, max = 75))
          startsCombat = checkForEvent()
        except:
          dialog = setError(message = "Can't start patrol mission.")
          return
        if not startsCombat:
          uMission = true
      of explore:
        try:
          updateGame(minutes = getRandom(min = 30, max = 60))
          startsCombat = checkForEvent()
        except:
          dialog = setError(message = "Can't start explore mission.")
          return
        if not startsCombat:
          uMission = true
  if startsCombat:
    closePopup()
    setCombat(state = state, dialog = dialog)
    return
  if uMission:
    try:
      updateMission(missionIndex = skyMap[playerShip.skyX][
          playerShip.skyY].missionIndex)
    except:
      dialog = setError(message = "Can't update the mission.")

proc deliverMedicines(dialog: var GameDialog; forFree: bool = true) {.raises: [
    ], tags: [RootEffect], contractual.} =
  ## Deliver medicines to a base
  ##
  ## * dialog  - the current in-game dialog displayed on the screen
  ## * forFree - if true, deliver the medicines for free to base, otherwise
  ##             request payment for them
  ##
  ## Returns the modified parameters dialog.
  closePopup()
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    eventIndex: int = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
    itemIndex: int = try:
        findItem(inventory = playerShip.cargo, itemType = factionsList[skyBases[
            baseIndex].owner].healingTools, itemQuality = any)
      except:
        dialog = setError(message = "Can't get index of medicines.")
        return
    event: EventData = eventsList[eventIndex]
    newTime: int = event.time - playerShip.cargo[itemIndex].amount
  if newTime < 1:
    deleteEvent(eventIndex = eventIndex)
  if forFree:
    try:
      gainRep(baseIndex = baseIndex, points = (playerShip.cargo[
          itemIndex].amount / 10).Natural)
    except:
      dialog = setError(message = "Can't gain reputation in base.")
      return
    try:
      addMessage(message = "You gave " & itemsList[playerShip.cargo[
          itemIndex].protoIndex].name & " for free to base.",
          mType = tradeMessage)
    except:
      dialog = setError(message = "Can't show message.")
      return
    updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
        itemIndex].protoIndex, amount = -(playerShip.cargo[itemIndex].amount),
        quality = playerShip.cargo[itemIndex].quality)
  else:
    try:
      gainRep(baseIndex = baseIndex, points = (playerShip.cargo[
          itemIndex].amount / 20).int * (-1))
    except:
      dialog = setError(message = "Can't gain reputation in base.")
      return
    try:
      sellItems(itemIndex = itemIndex, amount = $playerShip.cargo[
          itemIndex].amount)
    except TradeNoFreeCargoError:
      dialog = setMessage(message = "You can't sell medicines to the base because you don't have enough free cargo space for money.",
          title = "No free cargo space")
      return
    except NoMoneyInBaseError:
      dialog = setMessage(message = "You can't sell medicines to the base because the base don't have enough money to buy them.",
          title = "Can't sell medicines")
    except:
      dialog = setError(message = "Can't sell medicines to base.")

proc showShipOrders*(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the player's ship's orders menu
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current game's state
  ##
  ## Returns the modified parameters dialog.
  if dialog != ordersDialog:
    return
  try:
    let
      baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
          playerShip.skyY].baseIndex
      haveTrader: bool = findMember(order = talk) > -1
      height: float = countHeight(baseIndex = baseIndex,
          haveTrader = haveTrader, dialog = dialog).float
    const
      width: float = 200
    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Ship orders", x = dialogX, y = dialogY,
        w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30, cols = 1)
      if currentStory.index.len > 0:
        let step: StepData = try:
            (if currentStory.currentStep == -1: storiesList[
              currentStory.index].startingStep elif currentStory.currentStep >
              -1: storiesList[currentStory.index].steps[
              currentStory.currentStep] else: storiesList[
              currentStory.index].finalStep)
          except:
            dialog = setError(message = "Can't get the current story step.")
            return
        case step.finishCondition
        of askInBase:
          if baseIndex > 0:
            if currentStory.data.len == 0 or currentStory.data == skyBases[
                baseIndex].name:
              try:
                labelButton(title = "Ask for " & itemsList[getStepData(
                    finishData = step.finishData,
                    name = "item").parseInt].name):
                  executeStory(dialog = dialog, state = state)
              except:
                dialog = setError(message = "Can't add the story button.")
                return
        of destroyShip:
          let parts: seq[string] = currentStory.data.split(sep = ';')
          try:
            if playerShip.skyX == parts[0].parseInt and playerShip.skyY ==
                parts[1].parseInt:
              try:
                labelButton(title = "Search for " & protoShipsList[parts[
                    3].parseInt].name):
                  executeStory(dialog = dialog, state = state)
              except:
                dialog = setError(message = "Can't add the story button.")
                return
          except:
            dialog = setError(message = "Can't get the story step location.")
            return
        of explore:
          let parts: seq[string] = currentStory.data.split(sep = ';')
          try:
            if playerShip.skyX == parts[0].parseInt and playerShip.skyY ==
                parts[1].parseInt:
              labelButton(title = "Search area"):
                executeStory(dialog = dialog, state = state)
          except:
            dialog = setError(message = "Can't get the story step location.")
            return
        of any, loot:
          discard
      if playerShip.speed == docked:
        showDockedCommands(baseIndex = baseIndex, haveTrader = haveTrader,
            dialog = dialog, state = state)
      else:
        var event: EventsTypes = EventsTypes.none
        if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
          event = eventsList[skyMap[playerShip.skyX][
              playerShip.skyY].eventIndex].eType
        case event
        of enemyShip, enemyPatrol:
          labelButton(title = "Attack"):
            closePopup()
            setCombat(state = state, dialog = dialog)
        of fullDocks:
          labelButton(title = "Wait (full docks)"):
            closePopup()
            dialog = waitDialog
            setWaitMenu()
            setDialog()
        of attackOnBase:
          labelButton(title = "Defend"):
            closePopup()
            setCombat(state = state, dialog = dialog)
        of disease:
          if haveTrader:
            let itemIndex: int = try:
                findItem(inventory = playerShip.cargo,
                  itemType = factionsList[skyBases[
                      baseIndex].owner].healingTools, itemQuality = any)
              except:
                dialog = setError(message = "Can't find medicinet in the ship cargo.")
                return
            if itemIndex > -1:
              labelButton(title = "Deliver medicines for free"):
                deliverMedicines(dialog = dialog)
              labelButton(title = "Deliver medicines for price"):
                deliverMedicines(dialog = dialog, forFree = false)
        of EventsTypes.none, doublePrice, baseRecovery:
          if baseIndex > 0:
            if skyBases[baseIndex].reputation.level > -25:
              var dockingCost: int = 1
              for module in playerShip.modules:
                if module.mType == ModuleType2.hull:
                  dockingCost = module.maxModules
                  break
              if getBasePopulation(baseIndex = baseIndex) > empty:
                labelButton(title = "Dock (" & $dockingCost & " " & moneyName & ")"):
                  dockingOrder(dialog = dialog, state = state)
              else:
                labelButton(title = "Dock"):
                  dockingOrder(dialog = dialog, state = state)
            for mission in acceptedMissions:
              if haveTrader and mission.targetX == playerShip.skyX and
                  mission.targetY == playerShip.skyY and mission.finished:
                case mission.mType
                of deliver:
                  try:
                    labelButton(title = "Complete delivery of " &
                        itemsList[mission.itemIndex].name):
                      completeMission(dialog = dialog)
                  except:
                    dialog = setError(message = "Can't add accepted mission button.")
                    return
                of destroy:
                  try:
                    labelButton(title = "Complete destroy " &
                        protoShipsList[mission.shipIndex].name):
                      completeMission(dialog = dialog)
                  except:
                    dialog = setError(message = "Can't add accepted mission button.")
                    return
                of patrol:
                  labelButton(title = "Complete Patrol area mission"):
                    completeMission(dialog = dialog)
                of explore:
                  labelButton(title = "Complete Explore area mission"):
                    completeMission(dialog = dialog)
                of passenger:
                  labelButton(title = "Complete Transport passenger mission"):
                    completeMission(dialog = dialog)
          else:
            for mission in acceptedMissions:
              if mission.targetX == playerShip.skyX and mission.targetY ==
                  playerShip.skyY and not mission.finished:
                case mission.mType
                of deliver, passenger:
                  discard
                of destroy:
                  try:
                    labelButton(title = "Search for " &
                        protoShipsList[mission.shipIndex].name):
                      startMission(dialog = dialog, state = state)
                  except:
                    dialog = setError(message = "Can't add accepted mission button.")
                    return
                of patrol:
                  labelButton(title = "Patrol area"):
                    startMission(dialog = dialog, state = state)
                of explore:
                  labelButton(title = "Explore area"):
                    startMission(dialog = dialog, state = state)
        of trader:
          if haveTrader:
            labelButton(title = "Trade"):
              state = trade
              dialog = none
              closePopup()
              setTrade(dialog = dialog)
            labelButton(title = "Ask for events"):
              askForEvents(dialog = dialog)
              if dialog != none:
                return
            labelButton(title = "Ask for bases"):
              askForBases(dialog = dialog)
              if dialog != none:
                return
          labelButton(title = "Attack"):
            closePopup()
            setCombat(state = state, dialog = dialog)
        of friendlyShip:
          if haveTrader:
            try:
              if tradersName in protoShipsList[eventsList[skyMap[
                  playerShip.skyX][playerShip.skyY].eventIndex].shipIndex].name:
                labelButton(title = "Trade"):
                  state = trade
                  dialog = none
                  closePopup()
                  setTrade(dialog = dialog)
                labelButton(title = "Ask for bases"):
                  askForBases(dialog = dialog)
                  if dialog != none:
                    return
            except:
              dialog = setError(message = "Can't check if ship is trader.")
              return
            labelButton(title = "Ask for events"):
              askForEvents(dialog = dialog)
              if dialog != none:
                return
          labelButton(title = "Attack"):
            closePopup()
            setCombat(state = state, dialog = dialog)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")
