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
import ../[stories, tk]
import coreui

proc showStoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let
    frameName = mainPaned & ".knowledgeframe.stories.canvas.frame"
    storiesBox = frameName & ".options.titles"
    storyIndex = tclEval2(script = storiesBox & " current").parseInt
  var button = frameName & ".options.show"
  let
    lineWidth = ((tclEval2(script = "winfo reqwidth " & storiesBox).parseInt +
        tclEval2(script = "winfo reqwidth " & button).parseInt) / tclEval2(
        script = "font measure InterfaceFont { }").parseInt).int
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
    storyText.add(y = getCurrentStoryText() & '\n')
    rows = rows + (getCurrentStoryText().len / lineWidth).int + 1
    if currentStory.data.len > 0:
      let step = (if currentStory.currentStep == -1: storiesList[
          currentStory.index].startingStep elif currentStory.currentStep >
          -1: storiesList[currentStory.index].steps[
          currentStory.currentStep] else: storiesList[
          currentStory.index].finalStep)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the list of known stories
  try:
    discard
#    addCommand("ShowStory", showStoryCommand)
  except:
    showError(message = "Can't add a Tcl command.")
