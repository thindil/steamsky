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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to the information about the list of known stories,
## like sorting them, showing information about them, etc.

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[game, messages, stories, types]
import coreui, dialogs, errordialog, mapsui, setui

var
  storyIndex: Natural = 0
  storyText: string = ""

proc showStoriesInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the known stories
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # No stories discovered
  if knownStoriesList.len == 0:
    setLayoutRowDynamic(height = 100, cols = 1)
    wrapLabel(str = "You didn't discover any story yet.")
  else:
    setLayoutRowStatic(height = 30, cols = 3, ratio = [200.cfloat, 150, 250])
    let newStoryIndex = comboList(items = knownStoriesList,
        selected = storyIndex, itemHeight = 25, x = 150, y = 200)
    if newStoryIndex != storyIndex or storyText.len == 0:
      storyIndex = newStoryIndex
      storyText = ""
      let story: FinishedStoryData = finishedStories[storyIndex]
      for stepText in story.stepsTexts:
        storyText.add(y = stepText & '\n')
      if story.stepsTexts.len < story.stepsAmount:
        try:
          storyText.add(y = getCurrentStoryText() & '\n')
        except:
          dialog = setError(message = "Can't get current story text.")
          return
        if currentStory.data.len > 0:
          let
            step: StepData = try:
                (if currentStory.currentStep == -1: storiesList[
                  currentStory.index].startingStep elif currentStory.currentStep >
                  -1: storiesList[currentStory.index].steps[
                  currentStory.currentStep] else: storiesList[
                  currentStory.index].finalStep)
              except:
                dialog = setError(message = "Can't get the step.")
                return
            storyData: seq[string] = currentStory.data.split(sep = ';')
          case step.finishCondition
          of askInBase:
            if storyData.len < 2:
              storyText.add(y = "You must travel to base " & currentStory.data & " at X: ")
              for base in skyBases:
                if base.name == currentStory.data:
                  storyText.add(y = $base.skyX & " Y: " & $base.skyY)
                  break
            else:
              storyText.add(y = "You can ask in any base.")
          of destroyShip:
            try:
              storyText.add(y = "You must find " & protoShipsList[storyData[
                  2].parseInt].name & " at X: " & storyData[0] & " Y: " & storyData[1])
            except:
              dialog = setError(message = "Can't get the destroy ship step.")
              return
          of explore:
            storyText.add(y = "You must travel to X: " & storyData[0] & " Y: " &
                storyData[1])
          of loot:
            try:
              storyText.add(y = "You must loot: " & itemsList[storyData[
                  0].parseInt].name & " from ")
            except:
              dialog = setError(message = "Can't get the loot data.")
              return
            if storyData[1] == "any":
              storyText.add(y = "any ")
              let faction: FactionData = try:
                  factionsList[getStepData(finishData = step.finishData,
                    name = "faction")]
                except:
                  dialog = setError(message = "Can't get the faction")
                  return
              if faction.name.len > 0:
                storyText.add(y = faction.name)
              storyText.add(y = " ship.")
            else:
              for index, ship in protoShipsList:
                try:
                  if index == storyData[1].parseInt:
                    storyText.add(y = ship.name & ".")
                    break
                except:
                  dialog = setError(message = "Can't get the ship name.")
          of any:
            discard
    if currentStory.index.len > 0:
      labelButton(title = "Show on map"):
        var (newX, newY) = try:
            getStoryLocation()
          except:
            dialog = setError(message = "Can't get the story location.")
            return
        centerX = newX
        centerY = newY
        mapPreview = true
      labelButton(title = "Set as destination for ship"):
        var (newX, newY) = try:
            getStoryLocation()
          except:
            dialog = setError(message = "Can't get the story location.")
            return
        if newX == playerShip.skyX and newY == playerShip.skyY:
          dialog = setMessage(message = "You are at this location now.",
              title = "Can't set destination")
          return
        playerShip.destinationX = newX
        playerShip.destinationY = newY
        addMessage(message = "You set the travel destination for your ship.",
            mType = orderMessage)
    if storyText.len > 0:
      setLayoutRowDynamic(height = 200, cols = 1)
      wrapLabel(str = storyText)
