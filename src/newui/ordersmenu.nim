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
import ../[basestypes, game, maps, missions, shipscrew, stories, types, utils]
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
    discard
  if skyBases[baseIndex].population > 0:
    labelButton(title = "Escape"):
      discard
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
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")
