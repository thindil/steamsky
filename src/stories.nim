# Copyright 2023-2026 Bartek thindil Jasicki
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

## Provides code related to the game's stories, like reading them from a file,
## setting the current story, progressing, etc.

import std/[logging, paths, strutils, tables, xmlparser, xmltree]
import contracts
import events, game, log, maps, shipscargo, types, utils

type
  StartConditionType* = enum
    ## Types of requirements to start a story
    dropItem
  StepConditionType* = enum
    ## Types of requirements to finish a story step
    askInBase, destroyShip, explore, any, loot

  StepDataString* = string
    ## Used to store a story step data values

  StepTextData = object
    ## Used to store stories' steps' texts
    ##
    ## * condition - the requirement for the previous step of a story
    ## * text      - uhe text which will be shown to the player when the step starts
    condition*: StepConditionType
    text*: string

  StepFinishData = object
    ## Used to store information about requirements to finish a story's step
    ##
    ## * name  - the name of the data
    ## * value - the data's value
    name: string
    value: StepDataString

  StepData* = object
    ## Used to store information about stories' steps
    ##
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
    ##
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
    ##
    ## * index        - the index of the story or empty string if no story active
    ## * step         - the number of the step of the current story
    ## * currentStep  - the index of the current step of the story, 0 for starting
    ##                  step, -1 for finishing step
    ## * maxSteps     - the number of maximum amount of steps in the story
    ## * showText     - if true, show the text of the current step to the player
    ## * data         - various data for the current step of the story, depends on
    ##                  the step
    ## * finishedStep - the finish condition for the previous step in the story
    index*: string = ""
    step*: Positive = 1
    currentStep*: int = 0
    maxSteps*: Positive = 1
    showText*: bool = false
    data*: string = ""
    finishedStep*: StepConditionType = any

  FinishedStoryData* = object
    ## Used to store information about finished story
    ##
    ## * index       - the index of the finished story
    ## * stepsAmount - the amount of steps in the finished story
    ## * stepsTexts  - the texts of steps done in the story. If less than stepsAmount
    ##                 then it is the current story
    index*: string = ""
    stepsAmount*: Positive = 1
    stepsTexts*: seq[string] = @[]

var
  storiesList*: Table[string, StoryData] = initTable[string, StoryData]()
    ## The list of available stories in the game
  currentStory*: CurrentStoryData = CurrentStoryData(step: 1, maxSteps: 1)
    ## Contains data about the current story on which the player is
  finishedStories*: seq[FinishedStoryData] = @[]
    ## The list of finished stories

proc loadStep(step: XmlNode; story: var StoryData; storyIndex, startStep,
    finalStep: string; storyAction: DataAction) {.raises: [
    DataLoadingError], tags: [], contractual.} =
  ## Load the single story's step from a file
  ##
  ## * step        - the step which will be loaded
  ## * story       - the story to which the step will be loaded
  ## * storyIndex  - the index of the story
  ## * startStep   - the name of the starting step in the story
  ## * finalStep   - the name of the final step in the story
  ## * storyAction - the action which will be made with the story, like add,
  ##                 remove, etc
  ##
  ## Returns the modified parameter story.
  var tempStep: StepData = StepData(index: step.attr(name = "index"),
      finishCondition: askInBase)
  let stepAction: DataAction = try:
        parseEnum[DataAction](s = step.attr(name = "action").toLowerAscii)
      except ValueError:
        DataAction.add
  var stepIndex: int = -1
  for index, data in story.steps:
    if data.index == tempStep.index:
      stepIndex = index
      break
  if stepAction == remove:
    story.steps.delete(i = stepIndex)
  else:
    if stepAction == update:
      tempStep = story.steps[stepIndex]
    var attribute: string = step.attr(name = "finish")
    if attribute.len() > 0:
      tempStep.finishCondition = try:
        parseEnum[StepConditionType](s = step.attr(
            name = "finish").toLowerAscii)
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $storyAction & " story '" & $storyIndex & "', invalid step finish condition.")
    for stepData in step.findAll(tag = "finishdata"):
      let
        dataAction: DataAction = try:
            parseEnum[DataAction](s = stepData.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
        name: string = stepData.attr(name = "name")
      case dataAction
      of DataAction.add:
        tempStep.finishData.add(y = StepFinishData(name: name,
            value: stepData.attr(name = "value")))
      of update:
        for data in tempStep.finishData.mitems:
          if data.name == name:
            data.value = stepData.attr(name = "value")
      of remove:
        var deleteIndex: int = -1
        for index, data in tempStep.finishData:
          if data.name == name:
            deleteIndex = index
            break
        if deleteIndex > -1:
          tempStep.finishData.delete(i = deleteIndex)
    for text in step.findAll(tag = "text"):
      let
        textAction: DataAction = try:
            parseEnum[DataAction](s = text.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
        condition: StepConditionType = try:
            parseEnum[StepConditionType](s = text.attr(
                name = "condition"))
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $storyAction & " story '" &
                    $storyIndex & "', invalid text condition.")
      case textAction
      of DataAction.add:
        tempStep.texts.add(y = StepTextData(condition: condition,
            text: text.innerText()))
      of update:
        for stepText in tempStep.texts.mitems:
          if stepText.condition == condition:
            stepText.text = text.innerText()
      of remove:
        var deleteIndex: int = -1
        for index, data in tempStep.texts:
          if data.condition == condition:
            deleteIndex = index
            break
        if deleteIndex > -1:
          tempStep.texts.delete(i = deleteIndex)
    let failText: string = step.child(name = "failtext").innerText()
    if failText.len() > 0:
      tempStep.failText = failText
    if tempStep.index == startStep:
      story.startingStep = tempStep
    elif tempStep.index == finalStep:
      story.finalStep = tempStep
    else:
      if stepAction == DataAction.add:
        story.steps.add(y = tempStep)
      else:
        story.steps[stepIndex] = tempStep

proc loadStories*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the stories data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    ($fileName).len > 0
  body:
    let storiesXml: XmlNode = try:
        loadXml(path = $fileName)
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
            parseEnum[DataAction](s = storyNode.attr(
                name = "action").toLowerAscii)
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
            messageLevel = lvlInfo)
        continue
      var story: StoryData = if storyAction == DataAction.update:
          try:
            storiesList[storyIndex]
          except ValueError:
            StoryData(minSteps: 1, maxSteps: 1)
        else:
          StoryData(minSteps: 1, maxSteps: 1)
      var attribute: string = storyNode.attr(name = "name")
      if attribute.len() > 0:
        story.name = attribute
      let startStep: string = storyNode.attr(name = "startstep")
      let finalStep: string = storyNode.attr(name = "finalstep")
      attribute = storyNode.attr(name = "start")
      if attribute.len() > 0:
        story.startCondition = try:
            parseEnum[StartConditionType](s = attribute.toLowerAscii)
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
          value: string = startData.attr(name = "value")
          dataAction: DataAction = try:
              parseEnum[DataAction](s = startData.attr(
                  name = "action").toLowerAscii)
            except ValueError:
              DataAction.add
        case dataAction
        of DataAction.add:
          story.startData.add(y = value)
        of remove:
          var deleteIndex: int = -1
          for index, data in story.startData:
            if data == value:
              deleteIndex = index
              break
          if deleteIndex > -1:
            story.startData.delete(i = deleteIndex)
        of update:
          discard
      for faction in storyNode.findAll(tag = "forbiddenfaction"):
        let
          value: string = faction.attr(name = "value")
          factionAction: DataAction = try:
              parseEnum[DataAction](s = faction.attr(
                  name = "action").toLowerAscii)
            except ValueError:
              DataAction.add
        case factionAction
        of DataAction.add:
          story.forbiddenFactions.add(y = value)
        of remove:
          var deleteIndex: int = -1
          for index, data in story.forbiddenFactions:
            if data == value:
              deleteIndex = index
              break
          if deleteIndex > -1:
            story.forbiddenFactions.delete(i = deleteIndex)
        of update:
          discard
      for step in storyNode.findAll(tag = "step"):
        loadStep(step = step, story = story, storyIndex = storyIndex,
            startStep = startStep, finalStep = finalStep,
            storyAction = storyAction)
      let endText: string = storyNode.child(name = "endtext").innerText()
      if endText.len > 0:
        story.endText = endText
      if storyAction == DataAction.add:
        logMessage(message = "Story added: '" & $storyIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Story updated: '" & $storyIndex & "'",
            messageLevel = lvlInfo)
      storiesList[storyIndex] = story

proc selectBase*(value: string): string {.raises: [], tags: [],
    contractual.} =
  ## Selecte the name of a base for a story
  ##
  ## * value - only value "any" has matter, otherwise ignored
  ##
  ## Returns the name of a known and friendly base. If the value parameter is
  ## any, return an empty string.
  if value == "any":
    return ""
  while true:
    let baseIndex: BasesRange = getRandom(min = skyBases.low,
        max = skyBases.high)
    if skyBases[baseIndex].known and skyBases[baseIndex].reputation.level > -25:
      playerShip.destinationX = skyBases[baseIndex].skyX
      playerShip.destinationY = skyBases[baseIndex].skyY
      return skyBases[baseIndex].name

proc getStepData*(finishData: seq[StepFinishData];
    name: string): StepDataString {.raises: [], tags: [], contractual.} =
  ## Get the finishing data of the selected step based on its name
  ##
  ## * finishData - the list of the step's data
  ## * name       - the name of the data to find
  ##
  ## Returns the finishing data of the selected step with the selected name
  ## or empty string if nothing found.
  require:
    name.len > 0
  body:
    result = ""
    for data in finishData:
      if data.name == name:
        return data.value

proc selectLocation*(step: seq[StepFinishData]): string {.raises: [
    ValueError], tags: [], contractual.} =
  ## Get the location on the sky map for the story's step
  ##
  ## * step - the finishing data for the selected step
  ##
  ## Returns the string with X and Y coordinates for the selected step's location.
  var
    value: string = getStepData(finishData = step, name = "x")
    locationX, locationY: Natural = 1
  if value == "random":
    locationX = getRandom(min = MapXRange.low, max = MapXRange.high)
    result = $locationX & ";"
  else:
    locationX = value.parseInt
    result = value & ";"
  playerShip.destinationX = locationX
  value = getStepData(finishData = step, name = "y")
  if value == "random":
    while true:
      locationY = getRandom(min = MapYRange.low, max = MapYRange.high)
      if skyMap[locationX][locationY].baseIndex == 0 and locationY !=
          playerShip.skyY:
        break
    result = result & $locationY & ";"
  else:
    locationY = value.parseInt
    result = result & value & ";"
  playerShip.destinationY = locationY

proc selectEnemy*(step: seq[StepFinishData]): string {.raises: [
    ValueError], tags: [], contractual.} =
  ## Get the enemy ship for the selected story's step
  ##
  ## * step - the finishing data for the selected step
  ##
  ## Returns the string with X and Y coordinates and the index of the
  ## prototype's ship
  ensure:
    result.len > 0
  body:
    result = selectLocation(step = step)
    var value: string = getStepData(finishData = step, name = "ship")
    if value != "random":
      return result & value
    value = getStepData(finishData = step, name = "faction")
    var enemies: seq[Positive] = @[]
    generateEnemies(enemies = enemies, owner = value)
    return result & $enemies[getRandom(min = enemies.low, max = enemies.high)]

proc selectLoot*(step: seq[StepFinishData]): string {.raises: [
    KeyError], tags: [], contractual.} =
  ## Get the information about the item looted in this step of a story.
  ##
  ## * step - the finishing data for the selected step
  ##
  ## Returns the string with name of the item and index of the prototype's
  ## ship.
  ensure:
    result.len > 0
  body:
    result = getStepData(finishData = step, name = "item") & ";"
    var value: string = getStepData(finishData = step, name = "ship")
    if value != "random":
      return result & value
    value = getStepData(finishData = step, name = "faction")
    var enemies: seq[Positive] = @[]
    generateEnemies(enemies = enemies, owner = value)
    return result & $enemies[getRandom(min = enemies.low, max = enemies.high)]

proc startStory*(factionName: string; condition: StartConditionType) {.raises: [
    ValueError], tags: [], contractual.} =
  ## If possible, start a story
  ##
  ## * factionName - the name of faction which is needed to start the story
  ## * condition   - the starting condition of the story
  if currentStory.index.len > 0:
    return
  var factionIndex: string = ""
  for index, faction in factionsList:
    if faction.name == factionName:
      factionIndex = index
      break
  if factionIndex.len == 0:
    return
  var
    nextStory: bool = false
    step: string = ""
  for sIndex, story in storiesList:
    nextStory = false
    for forbiddenFaction in story.forbiddenFactions:
      if forbiddenFaction.toLowerAscii == playerShip.crew[
          0].faction.toLowerAscii:
        nextStory = true
        break
    if nextStory:
      continue
    if condition == dropItem:
      if story.startData[1] == factionIndex and getRandom(min = 1,
          max = story.startData[2].parseInt) == 1:
        case story.startingStep.finishCondition
        of askInBase:
          step = selectBase(value = getStepData(
              finishData = story.startingStep.finishData, name = "base"))
        of destroyShip:
          step = selectEnemy(step = story.startingStep.finishData)
        of explore:
          step = selectLocation(step = story.startingStep.finishData)
        of loot:
          step = selectLoot(step = story.startingStep.finishData)
        of any:
          discard
        currentStory = CurrentStoryData(index: sIndex, step: 1, currentStep: -1,
            maxSteps: getRandom(min = story.minSteps, max = story.maxSteps),
            showText: true, data: step, finishedStep: any)
        updateCargo(ship = playerShip, protoIndex = story.startData[0].parseInt,
            amount = 1, quality = normal)
        finishedStories.add(y = FinishedStoryData(index: currentStory.index,
            stepsAmount: currentStory.maxSteps, stepsTexts: @[]))
        return

proc getCurrentStoryText*(): string {.raises: [KeyError], tags: [],
    contractual.} =
  ## Get the text of the current step in the player's current story
  ##
  ## Returns the string with the current step text or empty string if nothing
  ## found.
  result = ""
  let stepTexts: seq[StepTextData] = if currentStory.currentStep == -1:
      storiesList[currentStory.index].startingStep.texts
    elif currentStory.currentStep > -1:
      storiesList[currentStory.index].steps[currentStory.currentStep].texts
    else:
      storiesList[currentStory.index].finalStep.texts
  for text in stepTexts:
    if text.condition == currentStory.finishedStep:
      return text.text

proc clearCurrentStory*() {.raises: [], tags: [], contractual.} =
  ## Reset the player's current story
  currentStory = CurrentStoryData()

proc getStoryLocation*(): tuple[storyX: MapXRange;
    storyY: MapYRange] {.raises: [ValueError], tags: [],
        contractual.} =
  ## Get the target's location of the current player's story
  ##
  ## Returns tuple with X and Y coordinates on the map of the target for
  ## the current story's step.
  result = (1, 1)
  if currentStory.data.len == 0:
    return (playerShip.skyX, playerShip.skyY)
  let coords: seq[string] = currentStory.data.split(sep = ';')
  if coords.len < 3:
    for skyBase in skyBases:
      if skyBase.name == currentStory.data:
        return (skyBase.skyX, skyBase.skyY)
  else:
    return (coords[1].parseInt.MapXRange, coords[2].parseInt.MapYRange)
