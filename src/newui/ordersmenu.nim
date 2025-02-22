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
import ../[game, maps, shipscrew, stories, types]
import coreui, errordialog

proc showDockedCommands(baseIndex: ExtendedBasesRange;
    haveTrader: bool) {.raises: [], tags: [], contractual.} =
  ## Show the available orders when the player's ship is docked to a base
  ##
  ## * baseIndex  - the index of the base to which the player's ship is docked
  ## * haveTrader - if true, someone in the crew is assigned to trader position
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
    const
      width: float = 200
      height: float = 80
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
      let haveTrader: bool = findMember(order = talk) > -1
      if playerShip.speed == docked:
        showDockedCommands(baseIndex = baseIndex, haveTrader = haveTrader)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")
