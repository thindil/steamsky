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

import std/[strutils, tables]
import ../[game, stories, tk]
import coreui

proc showStoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the current story information
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowStory
  let
    frameName = mainPaned & ".knowledgeframe.stories.canvas.frame"
    storiesBox = frameName & ".options.titles"
    storyIndex = try:
        tclEval2(script = storiesBox & " current").parseInt
      except:
        return showError(message = "Can't get story index.")
  var button = frameName & ".options.show"
  let
    lineWidth = try:
        ((tclEval2(script = "winfo reqwidth " & storiesBox).parseInt +
          tclEval2(script = "winfo reqwidth " & button).parseInt) / tclEval2(
          script = "font measure InterfaceFont { }").parseInt).int
      except:
        return showError(message = "Can't get line width.")
    storyView = frameName & ".view"
  tclEval(script = storyView & " configure -state normal -width " & $lineWidth)
  tclEval(script = storyView & " delete 1.0 end")
  var
    storyText = ""
    rows = 1
  let story = finishedStories[storyIndex]
  for stepText in story.stepsTexts:
    storyText.add(y = stepText & '\n')
    rows = rows + (stepText.len / lineWidth).int + 1
  if story.stepsTexts.len < story.stepsAmount:
    try:
      storyText.add(y = getCurrentStoryText() & '\n')
      rows = rows + (getCurrentStoryText().len / lineWidth).int + 1
    except:
      return showError(message = "Can't get current story text.")
    if currentStory.data.len > 0:
      let
        step = try:
            (if currentStory.currentStep == -1: storiesList[
              currentStory.index].startingStep elif currentStory.currentStep >
              -1: storiesList[currentStory.index].steps[
              currentStory.currentStep] else: storiesList[
              currentStory.index].finalStep)
          except:
            return showError(message = "Can't get the step.")
        storyData = currentStory.data.split(sep = ';')
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
          return showError(message = "Can't get the destroy ship step.")
      of explore:
        storyText.add(y = "You must travel to X: " & storyData[0] & " Y: " &
            storyData[1])
      of loot:
        try:
          storyText.add(y = "You must loot: " & itemsList[storyData[
              0].parseInt].name & " from ")
        except:
          return showError(message = "Can't get the loot data.")
        if storyData[1] == "any":
          storyText.add(y = "any ")
          let faction = try:
              factionsList[getStepData(finishData = step.finishData,
                name = "faction")]
            except:
              return showError(message = "Can't get the faction")
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
              return showError(message = "Can't get the ship name.")
      of any:
        discard
    tclEval(script = storyView & " insert end {" & storyText & "}")
    tclEval(script = "grid " & button)
    button = frameName & ".options.set"
    tclEval(script = "grid " & button)
  else:
    tclEval(script = "grid remove " & button)
    button = frameName & ".options.set"
    tclEval(script = "grid remove " & button)
  tclEval(script = storyView & " configure -state disabled -height " & $rows)
  return tclOk

proc showStoryLocationCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  var (newX, newY) = try:
      getStoryLocation()
    except:
      return showError(message = "Can't get the story location.")
  centerX = newX
  centerY = newY
  tclEval(script = "InvokeButton " & closeButton)
  tclEval(script = "grid remove " & closeButton)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the list of known stories
  try:
    discard
#    addCommand("ShowStory", showStoryCommand)
#    addCommand("ShowStoryLocation", showStoryLocationCommand)
  except:
    showError(message = "Can't add a Tcl command.")
