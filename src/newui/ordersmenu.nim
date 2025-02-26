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

## Provides code related to the player's ship's orders menu

import std/[tables, strutils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basestypes, crewinventory, game, maps, missions, shipscrew,
    shipsmovement, stories, types, utils]
import coreui, errordialog

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
    if skyBases[baseIndex].population > 0:
      result += 35
      if haveTrader and skyBases[baseIndex].population > 0:
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
    if skyBases[baseIndex].population == 0:
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
              itemType = factionsList[skyBases[baseIndex].owner].healingTools)
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

proc dockingOrder(escape: bool = false; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Docking, undocking and escaping from bases
  ##
  ## * escape  - if true, escape from a base
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened.
  var message: string = ""
  if playerShip.speed == docked:
    try:
      message = dockShip(docking = false, escape = escape)
    except:
      dialog = setError(message = "Can't undock from the base.")
      return
    if message.len > 0:
      # showMessage(text = message, title = "Can't undock from base")
      return
  else:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
      if eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].eType == fullDocks:
        return
    try:
      message = dockShip(docking = true)
    except:
      dialog = setError(message = "Can't dock to the base.")
    if message.len > 0:
      # showMessage(text = message, title = "Can't dock to base")
      return
  dialog = none
  closePopup()

proc showDockedCommands(baseIndex: ExtendedBasesRange;
    haveTrader: bool; dialog: var GameDialog) {.raises: [], tags: [RootEffect],
        contractual.} =
  ## Show the available orders when the player's ship is docked to a base
  ##
  ## * baseIndex  - the index of the base to which the player's ship is docked
  ## * haveTrader - if true, someone in the crew is assigned to trader position
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog if error happened.
  labelButton(title = "Undock"):
    dockingOrder(dialog = dialog)
  if skyBases[baseIndex].population > 0:
    labelButton(title = "Escape"):
      dockingOrder(escape = true, dialog = dialog)
    if haveTrader and skyBases[baseIndex].population > 0:
      labelButton(title = "Trade"):
        discard
      labelButton(title = "School"):
        discard
      if skyBases[baseIndex].recruits.len > 0:
        labelButton(title = "Recruit"):
          discard
    if daysDifference(dateToCompare = skyBases[baseIndex].askedForEvents) > 6:
      labelButton(title = "Ask for events"):
        discard
    if not skyBases[baseIndex].askedForBases:
      labelButton(title = "Ask for bases"):
        discard
    try:
      if "temple" in basesTypesList[skyBases[baseIndex].baseType].flags:
        labelButton(title = "Pray"):
          discard
    except:
      dialog = setError(message = "Can't check if base has temple flag.")
      return
    for member in playerShip.crew:
      if member.health < 100:
        labelButton(title = "Heal wounded"):
          discard
        break
    for module in playerShip.modules:
      if module.durability < module.maxDurability:
        labelButton(title = "Repair ship"):
          discard
        break
    try:
      if "shipyard" in basesTypesList[skyBases[baseIndex].baseType].flags:
        labelButton(title = "Shipyard"):
          discard
    except:
      dialog = setError(message = "Can't check if the base has shipyard flag.")
      return
    for index, recipe in recipesList:
      try:
        if index notin knownRecipes and index in basesTypesList[skyBases[
            baseIndex].baseType].recipes and recipe.reputation <= skyBases[
            baseIndex].reputation.level:
          labelButton(title = "Buy recipes"):
            discard
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
                discard
            except:
              dialog = setError(message = "Can't add mission button.")
              return
          of destroy:
            if mission.finished:
              try:
                labelButton(title = "Complete destroy " & protoShipsList[
                    mission.shipIndex].name):
                  discard
              except:
                dialog = setError(message = "Can't add mission button.")
                return
          of patrol:
            if mission.finished:
              labelButton(title = "Complete Patrol area mission"):
                discard
          of explore:
            if mission.finished:
              labelButton(title = "Complete Explore area mission"):
                discard
          of passenger:
            if mission.finished:
              labelButton(title = "Complete Transport passenger mission"):
                discard
        if mission.startBase == baseIndex:
          missionsLimit.dec
      if missionsLimit > 0:
        labelButton(title = "Missions"):
          discard
    if playerShip.homeBase != baseIndex:
      labelButton(title = "Set as home"):
        discard
  if skyBases[baseIndex].population == 0:
    labelButton(title = "Loot"):
      discard

proc showShipOrders*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the player's ship's orders menu
  ##
  ## * dialog - the current in-game dialog displayed on the screen
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
      setLayoutRowDynamic(30, 1)
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
                  discard
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
                  discard
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
                discard
          except:
            dialog = setError(message = "Can't get the story step location.")
            return
        of any, loot:
          discard
      if playerShip.speed == docked:
        showDockedCommands(baseIndex = baseIndex, haveTrader = haveTrader,
            dialog = dialog)
      else:
        var event: EventsTypes = EventsTypes.none
        if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
          event = eventsList[skyMap[playerShip.skyX][
              playerShip.skyY].eventIndex].eType
        case event
        of enemyShip, enemyPatrol:
          labelButton(title = "Attack"):
            discard
        of fullDocks:
          labelButton(title = "Wait (full docks)"):
            discard
        of attackOnBase:
          labelButton(title = "Defend"):
            discard
        of disease:
          if haveTrader:
            let itemIndex: int = try:
                findItem(inventory = playerShip.cargo,
                  itemType = factionsList[skyBases[
                      baseIndex].owner].healingTools)
              except:
                dialog = setError(message = "Can't find medicinet in the ship cargo.")
                return
            if itemIndex > -1:
              labelButton(title = "Deliver medicines for free"):
                discard
              labelButton(title = "Deliver medicines for price"):
                discard
        of EventsTypes.none, doublePrice, baseRecovery:
          if baseIndex > 0:
            if skyBases[baseIndex].reputation.level > -25:
              var dockingCost: int = 1
              for module in playerShip.modules:
                if module.mType == ModuleType2.hull:
                  dockingCost = module.maxModules
                  break
              if skyBases[baseIndex].population > 0:
                labelButton(title = "Dock (" & $dockingCost & " " & moneyName & ")"):
                  discard
              else:
                labelButton(title = "Dock"):
                  discard
            for mission in acceptedMissions:
              if haveTrader and mission.targetX == playerShip.skyX and
                  mission.targetY == playerShip.skyY and mission.finished:
                case mission.mType
                of deliver:
                  try:
                    labelButton(title = "Complete delivery of " &
                        itemsList[mission.itemIndex].name):
                      discard
                  except:
                    dialog = setError(message = "Can't add accepted mission button.")
                    return
                of destroy:
                  try:
                    labelButton(title = "Complete destroy " &
                        protoShipsList[mission.shipIndex].name):
                      discard
                  except:
                    dialog = setError(message = "Can't add accepted mission button.")
                    return
                of patrol:
                  labelButton(title = "Complete Patrol area mission"):
                    discard
                of explore:
                  labelButton(title = "Complete Explore area mission"):
                    discard
                of passenger:
                  labelButton(title = "Complete Transport passenger mission"):
                    discard
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
                      discard
                  except:
                    dialog = setError(message = "Can't add accepted mission button.")
                    return
                of patrol:
                  labelButton(title = "Patrol area"):
                    discard
                of explore:
                  labelButton(title = "Explore area"):
                    discard
        of trader:
          if haveTrader:
            labelButton(title = "Trade"):
              discard
            labelButton(title = "Ask for events"):
              discard
            labelButton(title = "Ask for bases"):
              discard
          labelButton(title = "Attack"):
            discard
        of friendlyShip:
          if haveTrader:
            try:
              if tradersName in protoShipsList[eventsList[skyMap[
                  playerShip.skyX][playerShip.skyY].eventIndex].shipIndex].name:
                labelButton(title = "Trade"):
                  discard
                labelButton(title = "Ask for bases"):
                  discard
            except:
              dialog = setError(message = "Can't check if ship is trader.")
              return
            labelButton(title = "Ask for events"):
              discard
          labelButton(title = "Attack"):
            discard
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")
