# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides code related to the list of known stories, like showing it,
## sorting or showing them on the map.

import std/[strutils, tables]
import contracts, nimalyzer
import ../[game, stories, tk, types]
import coreui, errordialog

proc showStoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual, ruleOff: "params".} =
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
    frameName: string = mainPaned & ".knowledgeframe.stories.canvas.frame"
    storiesBox: string = frameName & ".options.titles"
    storyIndex: int = try:
        tclEval2(script = storiesBox & " current").parseInt
      except:
        return showError(message = "Can't get story index.")
  var button: string = frameName & ".options.show"
  let
    lineWidth: Natural = try:
        ((tclEval2(script = "winfo reqwidth " & storiesBox).parseInt +
          tclEval2(script = "winfo reqwidth " & button).parseInt) / tclEval2(
          script = "font measure InterfaceFont { }").parseInt).int
      except:
        return showError(message = "Can't get line width.")
    storyView: string = frameName & ".view"
  tclEval(script = storyView & " configure -state normal -width " & $lineWidth)
  tclEval(script = storyView & " delete 1.0 end")
  var
    storyText: string = ""
    rows: Positive = 1
  let story: FinishedStoryData = finishedStories[storyIndex]
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
        step: StepData = try:
            (if currentStory.currentStep == -1: storiesList[
              currentStory.index].startingStep elif currentStory.currentStep >
              -1: storiesList[currentStory.index].steps[
              currentStory.currentStep] else: storiesList[
              currentStory.index].finalStep)
          except:
            return showError(message = "Can't get the step.")
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
          let faction: FactionData = try:
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
   argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
       TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show the current story event on map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowStoryLocation
  var (newX, newY) = try:
      getStoryLocation()
    except:
      return showError(message = "Can't get the story location.")
  tclEval(script = "ShowOnMap " & $newX & " " & $newY)
  return tclOk

proc setStoryCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
       TimeEffect, RootEffect], cdecl, contractual.} =
  ## Set the current story event as the player's ship destination
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetStory
  var (newX, newY) = try:
      getStoryLocation()
    except:
      return showError(message = "Can't get the story location.")
  tclEval(script = "SetDestination2 " & $newX & " " & $newY)
  return tclOk

proc updateStoriesList*() {.raises: [KeyError], tags: [RootEffect], contractual.} =
  ## Update the information about the list of known stories
  let knowledgeFrame: string = mainPaned & ".knowledgeframe.stories.canvas.frame"
  var rows: Natural = try:
      tclEval2(script = "grid size " & knowledgeFrame).split(sep = " ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = knowledgeFrame)
  if finishedStories.len == 0:
    let label: string = knowledgeFrame & ".nostories"
    tclEval(script = "ttk::label " & label & " -text {You didn't discover any story yet.} -wraplength 400")
    tclEval(script = "grid " & label & " -padx 10")
  else:
    var finishedStoriesList: string = ""
    for finishedStory in finishedStories:
      finishedStoriesList.add(y = "{ " & storiesList[
          finishedStory.index].name & "}")
    let optionsFrame: string = knowledgeFrame & ".options"
    tclEval(script = "ttk::frame " & optionsFrame)
    let storiesBox: string = optionsFrame & ".titles"
    tclEval(script = "ttk::combobox " & storiesBox &
        " -state readonly -values [list " & finishedStoriesList & "]")
    tclEval(script = "bind " & storiesBox & " <<ComboboxSelected>> ShowStory")
    tclEval(script = storiesBox & " current " & $finishedStories.high)
    tclEval(script = "grid " & storiesBox)
    var button: string = optionsFrame & ".show"
    tclEval(script = "ttk::button " & button & " -text {Show on map} -command ShowStoryLocation")
    tclEval(script = "grid " & button & " -column 1 -row 0")
    button = optionsFrame & ".set"
    tclEval(script = "ttk::button " & button & " -text {Set as destination for ship} -command SetStory")
    tclEval(script = "grid " & button & " -column 2 -row 0")
    tclEval(script = "grid " & optionsFrame & " -sticky w")
    let storiesView: string = knowledgeFrame & ".view"
    tclEval(script = "text " & storiesView & " -wrap word")
    tclEval(script = "grid " & storiesView & " -sticky w")
    tclEval(script = "event generate " & storiesBox & " <<ComboboxSelected>>")
  tclEval(script = "update")
  let knowledgeCanvas: string = mainPaned & ".knowledgeframe.stories.canvas"
  tclEval(script = knowledgeCanvas & " configure -scrollregion [list " &
      tclEval2(script = knowledgeCanvas & " bbox all") & "]")
  tclEval(script = knowledgeCanvas & " xview moveto 0.0")
  tclEval(script = knowledgeCanvas & " yview moveto 0.0")

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the list of known stories
  try:
    addCommand(name = "ShowStory", nimProc = showStoryCommand)
    addCommand(name = "ShowStoryLocation", nimProc = showStoryLocationCommand)
    addCommand(name = "SetStory", nimProc = setStoryCommand)
  except:
    showError(message = "Can't add a Tcl command.")
