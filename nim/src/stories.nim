# Copyright 2023 Bartek thindil Jasicki
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

import std/[strutils, tables, xmlparser, xmltree]
import game, log

type
  StartConditionType = enum
    ## Types of requirements to start a story
    dropItem
  StepConditionType = enum
    ## Types of requirements to finish a story step
    askInBase, destroyShip, explore, any, loot

  StepTextData = object
    ## Used to store stories' steps' texts
    ## * condition - the requirement for the previous step of a story
    ## * text      - uhe text which will be shown to the player when the step starts
    condition: StepConditionType
    text*: string

  StepFinishData = object
    ## Used to store information about requirements to finish a story's step
    ## * name  - the name of the data
    ## * value - the data's value
    name, value: string

  StepData = object
    ## Used to store information about stories' steps
    ## * index           - the index of the step
    ## * finishCondition - the condition which must be meet to finish the step and
    ##                     progress to the next step
    ## * finishData      - the data for the finish condition
    ## * texts           - the text which will be shown to the player when the step
    ##                     starts, depends on the finish condition of the previous
    ##                     step
    ## * failText        - the text which will be show to the player when the step
    ##                     fails
    index*: string
    finishCondition*: StepConditionType
    finishData*: seq[StepFinishData]
    texts*: seq[StepTextData]
    failText*: string

  StoryData = object
    ## Used to store information about a story
    ## * startCondition    - the condition which must be meet to start the story
    ## * startData         - the data of the starting condition
    ## * minSteps          - the minimal amount of steps in the story
    ## * maxSteps          - the maximum amount of steps in the story
    ## * startingStep      - the starting step of the story
    ## * steps             - contains all steps of the story
    ## * finalStep         - the final step of the story
    ## * endText           - the text which will be show to the player when the
    ##                       story ends
    ## * name              - the name of the story, show in the game
    ## * forbiddenFactions - if the player belongs to one of these factions, it
    ##                       can't start the story
    startCondition*: StartConditionType
    startData*: seq[string]
    minSteps*: Positive
    maxSteps*: Positive
    startingStep*: StepData
    steps*: seq[StepData]
    finalStep*: StepData
    endText*: string
    name*: string
    forbiddenFactions*: seq[string]

  CurrentStoryData = object
    ## Used to store information about the current story
    ## * index        - the index of the story or empty string if no story active
    ## * step         - the number of the step of the current story
    ## * currentStep  - the index of the current step of the story, 0 for starting
    ##                  step, -1 for finishing step
    ## * maxSteps     - the number of maximum amount of steps in the story
    ## * showText     - if true, show the text of the current step to the player
    ## * data         - various data for the current step of the story, depends on
    ##                  the step
    ## * finishedStep - the finish condition for the previous step in the story
    index*: string
    step*: Positive
    currentStep*: int
    maxSteps*: Positive
    showText*: bool
    data*: string
    finishedStep*: StepConditionType

  FinishedStoryData = object
    ## Used to store information about finished story
    ##
    ## * index       - the index of the finished story
    ## * stepsAmount - the amount of steps in the finished story
    ## * stepsTexts  - the texts of steps done in the story. If less than stepsAmount
    ##                 then it is the current story
    index*: string
    stepsAmount*: Positive
    stepsTexts*: seq[string]

var
  storiesList* = initTable[string, StoryData]() ## The list of available stories in the game
  currentStory*: CurrentStoryData = CurrentStoryData(step: 1,
      maxSteps: 1) ## Contains data about the current story on which the player is
  finishedStories*: seq[FinishedStoryData]

proc loadStories*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## Load the stories data from the file
  ##
  ## * fileName - the name of the file to load
  let storiesXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load stories data file. Reason: " &
          getCurrentExceptionMsg())
  for storyNode in storiesXml:
    if storyNode.kind != xnElement:
      continue
    let
      storyIndex: string = storyNode.attr(name = "index")
      storyAction: DataAction = try:
          parseEnum[DataAction](storyNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if storyAction in [update, remove]:
      if storyIndex notin storiesList:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $storyAction & " story '" & $storyIndex & "', there is no story with that index.")
    elif storyIndex in storiesList:
      raise newException(exceptn = DataLoadingError,
          message = "Can't add story '" & $storyIndex & "', there is an story with that index.")
    if storyAction == DataAction.remove:
      {.warning[ProveInit]: off.}
      {.warning[UnsafeDefault]: off.}
      storiesList.del(key = storyIndex)
      {.warning[ProveInit]: on.}
      {.warning[UnsafeDefault]: on.}
      logMessage(message = "story removed: '" & $storyIndex & "'",
          debugType = everything)
      continue
    var story: StoryData = if storyAction == DataAction.update:
        try:
          storiesList[storyIndex]
        except ValueError:
          StoryData(minSteps: 1, maxSteps: 1)
      else:
        StoryData(minSteps: 1, maxSteps: 1)
    var attribute = storyNode.attr(name = "name")
    if attribute.len() > 0:
      story.name = attribute
    let startStep = storyNode.attr(name = "startstep")
    let finalStep = storyNode.attr(name = "finalstep")
    attribute = storyNode.attr(name = "start")
    if attribute.len() > 0:
      story.startCondition = try:
          parseEnum[StartConditionType](attribute.toLowerAscii)
        except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $storyAction & " story '" & $storyIndex &
                "', invalid starting condition: '" & attribute & "'.")
    attribute = storyNode.attr(name = "minsteps")
    if attribute.len() > 0:
      story.minSteps = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $storyAction & " story '" & $storyIndex & "', invalid minimal amount of steps.")
    attribute = storyNode.attr(name = "maxsteps")
    if attribute.len() > 0:
      story.maxSteps = try:
          attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $storyAction & " story '" & $storyIndex & "', invalid maximum amount of steps.")
    for startData in storyNode.findAll(tag = "startdata"):
      let
        value = startData.attr(name = "value")
        dataAction: DataAction = try:
            parseEnum[DataAction](startData.attr(name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      case dataAction
      of DataAction.add:
        story.startData.add(value)
      of remove:
        var deleteIndex = -1
        for index, data in story.startData.pairs:
          if data == value:
            deleteIndex = index
            break
        if deleteIndex > -1:
          story.startData.delete(deleteIndex)
      of update:
        discard
    for faction in storyNode.findAll(tag = "forbiddenfaction"):
      let
        value = faction.attr(name = "value")
        factionAction: DataAction = try:
            parseEnum[DataAction](faction.attr(name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      case factionAction
      of DataAction.add:
        story.forbiddenFactions.add(value)
      of remove:
        var deleteIndex = -1
        for index, data in story.forbiddenFactions.pairs:
          if data == value:
            deleteIndex = index
            break
        if deleteIndex > -1:
          story.forbiddenFactions.delete(deleteIndex)
      of update:
        discard
    for step in storyNode.findAll(tag = "step"):
      var tempStep = StepData(index: step.attr(name = "index"),
          finishCondition: askInBase)
      let stepAction: DataAction = try:
            parseEnum[DataAction](step.attr(name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      var stepIndex = -1
      for index, data in story.steps.pairs:
        if data.index == tempStep.index:
          stepIndex = index
          break
      if stepAction == remove:
        story.steps.delete(stepIndex)
      else:
        if stepAction == update:
          tempStep = story.steps[stepIndex]
        attribute = step.attr(name = "finish")
        if attribute.len() > 0:
          tempStep.finishCondition = try:
            parseEnum[StepConditionType](step.attr(
                name = "finish").toLowerAscii)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $storyAction & " story '" & $storyIndex & "', invalid step finish condition.")
        for stepData in step.findAll(tag = "finishdata"):
          let
            dataAction: DataAction = try:
                parseEnum[DataAction](stepData.attr(
                    name = "action").toLowerAscii)
              except ValueError:
                DataAction.add
            name = stepData.attr(name = "name")
          case dataAction
          of DataAction.add:
            tempStep.finishData.add(StepFinishData(name: name,
                value: stepData.attr(name = "value")))
          of update:
            for data in tempStep.finishData.mitems:
              if data.name == name:
                data.value = stepData.attr(name = "value")
          of remove:
            var deleteIndex = -1
            for index, data in tempStep.finishData.pairs:
              if data.name == name:
                deleteIndex = index
                break
            if deleteIndex > -1:
              tempStep.finishData.delete(deleteIndex)
        for text in step.findAll(tag = "text"):
          let
            textAction: DataAction = try:
                parseEnum[DataAction](text.attr(name = "action").toLowerAscii)
              except ValueError:
                DataAction.add
            condition = try:
                parseEnum[StepConditionType](text.attr(name = "condition"))
              except ValueError:
                raise newException(exceptn = DataLoadingError,
                    message = "Can't " & $storyAction & " story '" &
                        $storyIndex & "', invalid text condition.")
          case textAction
          of DataAction.add:
            tempStep.texts.add(StepTextData(condition: condition,
                text: text.innerText()))
          of update:
            for stepText in tempStep.texts.mitems:
              if stepText.condition == condition:
                stepText.text = text.innerText()
          of remove:
            var deleteIndex = -1
            for index, data in tempStep.texts.pairs:
              if data.condition == condition:
                deleteIndex = index
                break
            if deleteIndex > -1:
              tempStep.texts.delete(deleteIndex)
        let failText = step.child(name = "failtext").innerText()
        if failText.len() > 0:
          tempStep.failText = failText
        if tempStep.index == startStep:
          story.startingStep = tempStep
        elif tempStep.index == finalStep:
          story.finalStep = tempStep
        else:
          if stepAction == DataAction.add:
            story.steps.add(tempStep)
          else:
            story.steps[stepIndex] = tempStep
    let endText = storyNode.child(name = "endtext").innerText()
    if endText.len > 0:
      story.endText = endText
    if storyAction == DataAction.add:
      logMessage(message = "Story added: '" & $storyIndex & "'",
          debugType = everything)
    else:
      logMessage(message = "Story updated: '" & $storyIndex & "'",
          debugType = everything)
    storiesList[storyIndex] = story

# Temporary code for interfacing with Ada

type
  AdaStepTextData = object
    condition: cint
    text: cstring

  AdaStepFinishData = object
    name, value: cstring

  AdaStepData = object
    index: cstring
    finishCondition: cint
    finishData: array[10, AdaStepFinishData]
    texts: array[10, AdaStepTextData]
    failText: cstring

  AdaStoryData = object
    startCondition: cint
    startData: array[10, cstring]
    minSteps: cint
    maxSteps: cint
    startingStep: AdaStepData
    steps: array[10, AdaStepData]
    finalStep: AdaStepData
    endText: cstring
    name: cstring
    forbiddenFactions: array[10, cstring]

  AdaCurrentStoryData = object
    index: cstring
    step: cint
    currentStep: cint
    maxSteps: cint
    showText: cint
    data: cstring
    finishedStep: cint

  AdaFinishedStoryData = object
    index: string
    stepsAmount: cint
    stepsTexts: array[10, string]

proc loadAdaStories(fileName: cstring): cstring {.sideEffect, raises: [],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  try:
    loadStories(fileName = $fileName)
    return "".cstring
  except DataLoadingError:
    return getCurrentExceptionMsg().cstring

proc getAdaStory(index: cstring; adaStory: var AdaStoryData) {.sideEffect,
    raises: [], tags: [], exportc.} =
  adaStory = AdaStoryData(startCondition: -1, minSteps: -1, maxSteps: -1)
  let recipeKey = strip(s = $index)
  if not storiesList.hasKey(key = recipeKey):
    return
  let story = try:
      storiesList[recipeKey]
    except KeyError:
      return
  adaStory.startCondition = story.startCondition.ord.cint
  for data in adaStory.startData.mitems:
    data = "".cstring
  for index, data in story.startData.pairs:
    adaStory.startData[index] = data.cstring
  adaStory.minSteps = story.minSteps.cint
  adaStory.maxSteps = story.maxSteps.cint

  proc convertStep(step: StepData): AdaStepData =
    result = AdaStepData(index: step.index.cstring,
        finishCondition: step.finishCondition.ord.cint,
        failText: step.failText.cstring)
    for index, data in result.finishData.mpairs:
      data = AdaStepFinishData(name: "".cstring, value: "".cstring)
      result.texts[index] = AdaStepTextData(condition: -1, text: "".cstring)
    for index, data in step.finishData.pairs:
      result.finishData[index] = AdaStepFinishData(name: data.name.cstring,
          value: data.value.cstring)
    for index, data in step.texts.pairs:
      result.texts[index] = AdaStepTextData(condition: data.condition.ord.cint,
          text: data.text.cstring)

  adaStory.startingStep = convertStep(story.startingStep)
  for index, step in adaStory.steps.mpairs:
    step.index = "".cstring
  for index, step in story.steps.pairs:
    adaStory.steps[index] = convertStep(step)
  adaStory.finalStep = convertStep(story.finalStep)
  adaStory.endText = story.endText.cstring
  adaStory.name = story.name.cstring
  for faction in adaStory.forbiddenFactions.mitems:
    faction = "".cstring
  for index, faction in story.forbiddenFactions.pairs:
    adaStory.forbiddenFactions[index] = faction.cstring

proc getAdaCurrentStory(story: AdaCurrentStoryData) {.sideEffect, raises: [],
    tags: [], exportc.} =
  currentStory = CurrentStoryData(index: $story.index, step: story.step,
      currentStep: story.currentStep, maxSteps: story.maxSteps,
      showText: story.showText == 1, data: $story.data,
      finishedStep: story.finishedStep.StepConditionType)

proc getAdaFinishedStory(index: cint; story: AdaFinishedStoryData) {.sideEffect,
    raises: [], tags: [], exportc.} =
  if story.index.len == 0:
    return
  var finishedStory = FinishedStoryData(index: story.index,
      stepsAmount: story.stepsAmount)
  for text in story.stepsTexts:
    finishedStory.stepsTexts.add(y = $text)
  if index <= finishedStories.len:
    finishedStories[index - 1] = finishedStory
  else:
    finishedStories.add(y = finishedStory)