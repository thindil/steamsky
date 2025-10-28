# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides code related to saving and loading the game state from file.

import std/[logging, os, strutils, tables, xmltree, xmlparser]
import contracts
import basessaveload, config, game, goals, log, maps, messages, missions,
    reputation, shipssaveload, statistics, stories, types, utils

const saveVersion: Positive = 6 ## The current version of the game saves files

type SaveGameInvalidData* = object of CatchableError
  ## Raised when there is an invalid data in the saved game file.

var saveName*: string = "" ## The full path to the game save file with file name

proc saveGame*(prettyPrint: bool = false) {.raises: [KeyError,
    IOError], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Save the game data to the file.
  ##
  ## * prettyPrint - if true, format properly XML before save, default is false,
  ##                 for reduce size of the file
  logMessage(message = "Start saving game in file " & saveName & ".",
      messageLevel = lvlInfo)
  var saveTree: XmlNode = newXmlTree(tag = "save", children = [], attributes = {
      "version": $saveVersion}.toXmlAttributes)
  logMessage(message = "Saving game difficulty settings...",
      messageLevel = lvlInfo)
  let difficulties: array[8, tuple[key: string, val: BonusType]] = [(
      "enemydamagebonus", newGameSettings.enemyDamageBonus), (
      "playerdamagebonus", newGameSettings.playerDamageBonus), (
      "enemymeleedamagebonus", newGameSettings.enemyMeleeDamageBonus), (
      "playermeleedamagebonus", newGameSettings.playerMeleeDamageBonus), (
      "experiencebonus", newGameSettings.experienceBonus), ("reputationbonus",
      newGameSettings.reputationBonus), ("upgradecostbonus",
      newGameSettings.upgradeCostBonus), ("pricesbonus",
      newGameSettings.pricesBonus)]
  var
    diffElement: XmlNode = newElement(tag = "difficulty")
    attrs: seq[tuple[key, val: string]] = @[]
  for difficulty in difficulties:
    attrs.add(y = (difficulty[0], $difficulty[1]))
  diffElement.attrs = attrs.toXmlAttributes
  saveTree.add(son = diffElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving game time...", messageLevel = lvlInfo)
  var dateElement: XmlNode = newElement(tag = "gamedate")
  dateElement.attrs = {"year": $gameDate.year, "month": $gameDate.month,
      "day": $gameDate.day, "hour": $gameDate.hour,
      "minutes": $gameDate.minutes}.toXmlAttributes
  saveTree.add(son = dateElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving map...", messageLevel = lvlInfo)
  for x in MapXRange.low..MapXRange.high:
    for y in MapYRange.low..MapYRange.high:
      if skyMap[x][y].visited:
        var fieldElement: XmlNode = newElement(tag = "field")
        fieldElement.attrs = {"x": $x, "y": $y}.toXmlAttributes
        saveTree.add(son = fieldElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving bases...", messageLevel = lvlInfo)
  saveBases(saveData = saveTree)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving player ship...", messageLevel = lvlInfo)
  savePlayerShip(saveData = saveTree)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving known recipes...", messageLevel = lvlInfo)
  for recipe in knownRecipes:
    var recipeElement: XmlNode = newElement(tag = "recipe")
    recipeElement.attrs = {"index": recipe}.toXmlAttributes
    saveTree.add(son = recipeElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving messages...", messageLevel = lvlInfo)
  let messagesToSave: Natural = (if gameSettings.savedMessages > messagesAmount(): messagesAmount() else: gameSettings.savedMessages)
  for i in (messagesAmount() - messagesToSave + 1)..messagesAmount():
    let message: MessageData = getMessage(messageIndex = i)
    var messageElement: XmlNode = newElement(tag = "message")
    messageElement.attrs = {"type": $message.kind,
        "color": $message.color}.toXmlAttributes
    messageElement.add(son = newText(text = $message.message))
    saveTree.add(son = messageElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving events...", messageLevel = lvlInfo)
  for event in eventsList:
    var
      eventElement: XmlNode = newElement(tag = "event")
      eventData: string = ""
    case event.eType
    of doublePrice:
      eventData = $event.itemIndex
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      eventData = $event.shipIndex
    else:
      eventData = $event.data
    eventElement.attrs = {"data": eventData, "type": $event.eType.ord,
        "x": $event.skyX, "y": $event.skyY, "time": $event.time}.toXmlAttributes
    saveTree.add(son = eventElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving game statistics...", messageLevel = lvlInfo)
  var statsElement: XmlNode = newElement(tag = "statistics")
  statsElement.attrs = {"visitedbases": $gameStats.basesVisited,
      "mapdiscovered": $gameStats.mapVisited,
      "distancetraveled": $gameStats.distanceTraveled,
      "acceptedmissions": $gameStats.acceptedMissions,
      "points": $gameStats.points}.toXmlAttributes

  proc saveStatistics(stats: seq[StatisticsData],
      statName: string) {.raises: [], tags: [], contractual.} =
    ## Save the selected game's statistic to the file
    ##
    ## * stats    - the game's statistics
    ## * statName - the name of the statistic to save
    require:
      statName.len > 0
    body:
      for statistic in stats:
        var statElement: XmlNode = newElement(tag = statName)
        statElement.attrs = {"index": statistic.index,
            "amount": $statistic.amount}.toXmlAttributes
        statsElement.add(son = statElement)

  saveStatistics(stats = gameStats.destroyedShips, statName = "destroyedships")
  saveStatistics(stats = gameStats.craftingOrders, statName = "finishedcrafts")
  saveStatistics(stats = gameStats.finishedMissions,
      statName = "finishedmissions")
  saveStatistics(stats = gameStats.finishedGoals, statName = "finishedgoals")
  saveStatistics(stats = gameStats.killedMobs, statName = "killedmobs")
  saveTree.add(son = statsElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving current goal...", messageLevel = lvlInfo)
  var goalElement: XmlNode = newElement(tag = "currentgoal")
  goalElement.attrs = {"index": $currentGoal.index,
      "type": $currentGoal.goalType.ord, "amount": $currentGoal.amount,
      "target": currentGoal.targetIndex,
      "multiplier": $currentGoal.multiplier}.toXmlAttributes
  saveTree.add(son = goalElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  if currentStory.index.len > 0:
    logMessage(message = "Saving current story...", messageLevel = lvlInfo)
    var
      storyElement: XmlNode = newElement(tag = "currentstory")
      attrs2: seq[tuple[key, val: string]] = @[]
    attrs2.add(y = ("index", currentStory.index))
    case currentStory.currentStep
    of 0:
      attrs2.add(y = ("currentstep", "start"))
    of -1:
      attrs2.add(y = ("currentstep", "finish"))
    else:
      attrs2.add(y = ("currentstep", storiesList[currentStory.index].steps[
          currentStory.currentStep].index))
    attrs2.add(y = ("maxsteps", $currentStory.maxSteps))
    if currentStory.showText:
      attrs2.add(y = ("showText", "Y"))
    else:
      attrs2.add(y = ("showText", "N"))
    if currentStory.data.len > 0:
      attrs2.add(y = ("data", currentStory.data))
    attrs2.add(y = ("finishedstep", $currentStory.finishedStep.ord))
    storyElement.attrs = attrs2.toXmlAttributes
    saveTree.add(son = storyElement)
    logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving finished stories...", messageLevel = lvlInfo)
  for finishedStory in finishedStories:
    var
      storyNode: XmlNode = newXmlTree(tag = "finishedstory", children = [],
          attributes = {
        "index": finishedStory.index,
        "stepsamount": $finishedStory.stepsAmount}.toXmlAttributes)
    for text in finishedStory.stepsTexts:
      var textElement: XmlNode = newElement(tag = "steptext")
      textElement.add(son = newText(text = text))
      storyNode.add(son = textElement)
    saveTree.add(son = storyNode)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving accepted missions...", messageLevel = lvlInfo)
  for mission in acceptedMissions:
    var
      missionElement: XmlNode = newElement(tag = "acceptedmission")
      attrs2: seq[tuple[key, val: string]] = @[]
    attrs2.add(y = ("type", $mission.mType.ord))
    case mission.mType
    of deliver:
      attrs2.add(y = ("target", $mission.itemIndex))
    of passenger:
      attrs2.add(y = ("target", $mission.data))
    of destroy:
      attrs2.add(y = ("target", $mission.shipIndex))
    else:
      attrs2.add(y = ("target", $mission.target))
    attrs2.add(y = ("time", $mission.time))
    attrs2.add(y = ("targetx", $mission.targetX))
    attrs2.add(y = ("targety", $mission.targetY))
    attrs2.add(y = ("reward", $mission.reward))
    attrs2.add(y = ("startbase", $mission.startBase))
    if mission.finished:
      attrs2.add(y = ("finished", "Y"))
    else:
      attrs2.add(y = ("finished", "N"))
    if mission.multiplier != 1.0:
      attrs2.add(y = ("multiplier", $mission.multiplier))
    missionElement.attrs = attrs2.toXmlAttributes()
    saveTree.add(son = missionElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving player career...", messageLevel = lvlInfo)
  var careerElement: XmlNode = newElement(tag = "playercareer")
  careerElement.attrs = {"index": playerCareer}.toXmlAttributes()
  saveTree.add(son = careerElement)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Saving player reputation...", messageLevel = lvlInfo)
  saveReputation(saveTree = saveTree)
  logMessage(message = "done", messageLevel = lvlInfo)
  var saveText: string = $saveTree
  if not prettyPrint:
    var lines: seq[string] = saveText.splitLines
    for line in lines.mitems:
      line = line.strip
    saveText = lines.join
  writeFile(filename = saveName, content = saveText)
  logMessage(message = "Finished saving game in file " & saveName & ".",
      messageLevel = lvlInfo)

proc loadGame*() {.raises: [IOError, OSError, ValueError,
    Exception], tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the game from a file
  let savedGame: XmlNode = loadXml(path = saveName)
  logMessage(message = "Start loading game from file " & saveName & ".",
      messageLevel = lvlInfo)
  # Check the same game compatybility
  if savedGame.attr(name = "version").parseInt > saveVersion:
    raise newException(exceptn = DataLoadingError,
        message = "This save is incompatible with this version of the game.")
  # Load the game difficulty settings
  logMessage(message = "Loading the game difficulty settings...",
      messageLevel = lvlInfo)
  var diffNode: XmlNode = savedGame.child(name = "difficulty")
  newGameSettings.enemyDamageBonus = diffNode.attr(name =
    "enemydamagebonus").parseFloat
  newGameSettings.playerDamageBonus = diffNode.attr(name =
    "playerdamagebonus").parseFloat
  newGameSettings.enemyMeleeDamageBonus = diffNode.attr(name =
    "enemymeleedamagebonus").parseFloat
  newGameSettings.playerMeleeDamageBonus = diffNode.attr(name =
    "playermeleedamagebonus").parseFloat
  newGameSettings.experienceBonus = diffNode.attr(
      name = "experiencebonus").parseFloat
  newGameSettings.reputationBonus = diffNode.attr(
      name = "reputationbonus").parseFloat
  newGameSettings.upgradeCostBonus = diffNode.attr(name =
    "upgradecostbonus").parseFloat
  newGameSettings.pricesBonus = diffNode.attr(name = "pricesbonus").parseFloat
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the game date
  logMessage(message = "Loading the game time...", messageLevel = lvlInfo)
  var dateNode: XmlNode = savedGame.child(name = "gamedate")
  gameDate.year = dateNode.attr(name = "year").parseInt
  gameDate.month = dateNode.attr(name = "month").parseInt
  gameDate.day = dateNode.attr(name = "day").parseInt
  gameDate.hour = dateNode.attr(name = "hour").parseInt
  gameDate.minutes = dateNode.attr(name = "minutes").parseInt
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the sky map
  logMessage(message = "Loading the map...", messageLevel = lvlInfo)
  for x in 1..1_024:
    for y in 1..1_024:
      skyMap[x][y].missionIndex = -1
      skyMap[x][y].baseIndex = 0
      skyMap[x][y].eventIndex = -1
      skyMap[x][y].visited = false
  for field in savedGame.findAll(tag = "field"):
    let
      x: MapXRange = field.attr(name = "x").parseInt
      y: MapYRange = field.attr(name = "y").parseInt
    skyMap[x][y].visited = true
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load sky bases
  logMessage(message = "Loading bases...", messageLevel = lvlInfo)
  loadBases(saveData = savedGame)
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the player's ship
  logMessage(message = "Loading the player's ship...", messageLevel = lvlInfo)
  loadPlayerShip(saveData = savedGame)
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load known recipes
  logMessage(message = "Loading known recipes...", messageLevel = lvlInfo)
  for recipe in savedGame.findAll(tag = "recipe"):
    knownRecipes.add(y = recipe.attr(name = "index"))
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load messages
  logMessage(message = "Loading messages...", messageLevel = lvlInfo)
  for message in savedGame.findAll(tag = "message"):
    restoreMessage(message = message.innerText, kind = parseEnum[MessageType](
        s = message.attr(name = "type")), color = parseEnum[MessageColor](
        s = message.attr(name = "color")))
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load events
  logMessage(message = "Loading events...", messageLevel = lvlInfo)
  for index, savedEvent in savedGame.findAll(tag = "event"):
    var event: EventData = EventData(skyX: savedEvent.attr(name = "x").parseInt,
        skyY: savedEvent.attr(name = "y").parseInt, time: savedEvent.attr(
        name = "time").parseInt, eType: savedEvent.attr(
            name = "type").parseInt.EventsTypes)
    case event.eType
      of doublePrice:
        event.itemIndex = savedEvent.attr(name = "data").parseInt
      of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
        event.shipIndex = savedEvent.attr(name = "data").parseInt
      else:
        event.data = savedEvent.attr(name = "data").parseInt
    eventsList.add(y = event)
    skyMap[event.skyX][event.skyY].eventIndex = index
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the current story
  let storyNode: XmlNode = savedGame.child(name = "currentstory")
  if storyNode != nil:
    logMessage(message = "Loading the current story...", messageLevel = lvlInfo)
    currentStory.index = storyNode.attr(name = "index")
    currentStory.step = storyNode.attr(name = "step").parseInt
    case storyNode.attr(name = "currentstep")
    of "start":
      currentStory.currentStep = 0
    of "finish":
      currentStory.currentStep = -1
    else:
      for index, step in storiesList[currentStory.index].steps:
        if step.index == storyNode.attr(name = "currentStep"):
          currentStory.currentStep = index
          break
    currentStory.maxSteps = storyNode.attr(name = "maxsteps").parseInt
    currentStory.showText = storyNode.attr(name = "showtext") == "Y"
    currentStory.finishedStep = storyNode.attr(name =
      "finishedstep").parseInt.StepConditionType
    logMessage(message = "done", messageLevel = lvlInfo)
  # Load finished stories data
  logMessage(message = "Loading finished stories...", messageLevel = lvlInfo)
  for savedStory in savedGame.findAll(tag = "finishedstory"):
    var story: FinishedStoryData = FinishedStoryData()
    story.index = savedStory.attr(name = "index")
    story.stepsAmount = savedStory.attr(name = "stepsamount").parseInt
    for text in savedStory:
      story.stepsTexts.add(y = text.innerText)
    finishedStories.add(y = story)
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load accepted missions
  logMessage(message = "Loading accepted missions...", messageLevel = lvlInfo)
  for index, mission in savedGame.findAll(tag = "acceptedmission"):
    var tmpMission: MissionData = MissionData(mtype: mission.attr(name =
      "type").parseInt.MissionsTypes, time: mission.attr(
          name = "time").parseInt,
      targetX: mission.attr(name = "targetx").parseInt, targetY: mission.attr(name =
      "targety").parseInt, reward: mission.attr(name = "reward").parseInt,
      startBase: mission.attr(name = "startbase").parseInt,
          finished: mission.attr(name =
      "finished") == "Y", multiplier: 1.0)
    case tmpMission.mType
    of deliver:
      tmpMission.itemIndex = mission.attr(name = "target").parseInt
    of passenger:
      tmpMission.data = mission.attr(name = "target").parseInt
    of destroy:
      tmpMission.shipIndex = mission.attr(name = "target").parseInt
    else:
      tmpMission.target = mission.attr(name = "target").parseInt
    let multiplier: string = mission.attr(name = "multiplier")
    if multiplier.len > 0:
      tmpMission.multiplier = multiplier.parseFloat
    acceptedMissions.add(y = tmpMission)
    skyMap[tmpMission.targetX][tmpMission.targetY].missionIndex = index
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load game statistics
  logMessage(message = "Loading game statistics...", messageLevel = lvlInfo)
  for stat in savedGame.findAll(tag = "statistics"):
    gameStats.basesVisited = stat.attr(name = "visitedbases").parseInt
    gameStats.mapVisited = stat.attr(name = "mapdiscovered").parseInt
    gameStats.distanceTraveled = stat.attr(name = "distancetraveled").parseInt
    gameStats.acceptedMissions = stat.attr(name = "acceptedmissions").parseInt
    gameStats.points = stat.attr(name = "points").parseInt
  for item in savedGame.findAll(tag = "destroyedships"):
    gameStats.destroyedShips.add(y = StatisticsData(index: item.attr(
        name = "index"), amount: item.attr(name = "amount").parseInt))
  for item in savedGame.findAll(tag = "finishedcrafts"):
    gameStats.craftingOrders.add(y = StatisticsData(index: item.attr(
        name = "index"), amount: item.attr(name = "amount").parseInt))
  for item in savedGame.findAll(tag = "finishedmissions"):
    gameStats.finishedMissions.add(y = StatisticsData(index: item.attr(
        name = "index"), amount: item.attr(name = "amount").parseInt))
  for item in savedGame.findAll(tag = "finishedgoals"):
    gameStats.finishedGoals.add(y = StatisticsData(index: item.attr(
        name = "index"), amount: item.attr(name = "amount").parseInt))
  for item in savedGame.findAll(tag = "killedmobs"):
    gameStats.killedMobs.add(y = StatisticsData(index: item.attr(
        name = "index"), amount: item.attr(name = "amount").parseInt))
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the player's current goal
  logMessage(message = "Loading game current goal...", messageLevel = lvlInfo)
  var goalNode: XmlNode = savedGame.child(name = "currentgoal")
  currentGoal = GoalData(index: goalNode.attr(name = "index"),
      goalType: goalNode.attr(name =
    "type").parseInt.GoalTypes, amount: goalNode.attr(name = "amount").parseInt,
    targetIndex: goalNode.attr(name = "target"), multiplier: goalNode.attr(name =
    "multiplier").parseInt)
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the player's career
  logMessage(message = "Loading the player's career...", messageLevel = lvlInfo)
  let careerNode: XmlNode = savedGame.child(name = "playercareer")
  playerCareer = careerNode.attr(name = "index")
  logMessage(message = "done", messageLevel = lvlInfo)
  # Load the factions' reputation
  logMessage(message = "Loading the player's reputation...", messageLevel = lvlInfo)
  loadReputation(savedGame = savedGame)
  logMessage(message = "done", messageLevel = lvlInfo)
  logMessage(message = "Finished loading the game.", messageLevel = lvlInfo)

proc generateSaveName*(renameSave: bool = false) {.raises: [OSError,
    IOError, Exception], tags: [ReadDirEffect, WriteIOEffect, ReadIOEffect],
        contractual.} =
  ## Generate an unique name for the save game file
  ##
  ## * renameSave - if true, rename the existing save game file.
  let oldSaveName: string = saveName
  while true:
    saveName = saveDirectory & playerShip.crew[0].name & "_" & playerShip.name &
        "_" & $getRandom(min = 100, max = 999) & ".sav"
    if not fileExists(filename = saveName):
      break
  if renameSave:
    if fileExists(filename = oldSaveName):
      moveFile(source = oldSaveName, dest = saveName)
