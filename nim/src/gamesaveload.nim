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

import std/[os, strutils, tables, xmltree, xmlparser]
import basessaveload, config, game, goals, log, maps, messages, missions,
    shipssaveload, statistics, stories, types, utils

const saveVersion = 5 ## The current version of the game saves files

type SaveGameInvalidData* = object of CatchableError
  ## Raised when there is an invalid data in the saved game file.

var saveName*: string ## The full path to the game save file with file name

proc saveGame*(prettyPrint: bool = false) {.sideEffect, raises: [KeyError,
    IOError], tags: [WriteIOEffect, RootEffect].} =
  ## Save the game data to the file.
  ##
  ## * prettyPrint - if true, format properly XML before save, default is false,
  ##                 for reduce size of the file
  logMessage(message = "Start saving game in file " & saveName & ".",
      debugType = everything)
  var saveTree = newXmlTree("save", [], {
      "version": $saveVersion}.toXmlAttributes)
  logMessage(message = "Saving game difficulty settings...",
      debugType = everything)
  let difficulties = [("enemydamagebonus", newGameSettings.enemyDamageBonus), (
      "playerdamagebonus", newGameSettings.playerDamageBonus), (
      "enemymeleedamagebonus", newGameSettings.enemyMeleeDamageBonus), (
      "playermeleedamagebonus", newGameSettings.playerMeleeDamageBonus), (
      "experiencebonus", newGameSettings.experienceBonus), ("reputationbonus",
      newGameSettings.reputationBonus), ("upgradecostbonus",
      newGameSettings.upgradeCostBonus), ("pricesbonus",
      newGameSettings.pricesBonus)]
  var
    diffElement = newElement("difficulty")
    attrs: seq[tuple[key, val: string]] = @[]
  for difficulty in difficulties:
    attrs.add((difficulty[0], $difficulty[1]))
  diffElement.attrs = attrs.toXmlAttributes
  saveTree.add(diffElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving game time...", debugType = everything)
  var dateElement = newElement("gamedate")
  dateElement.attrs = {"year": $gameDate.year, "month": $gameDate.month,
      "day": $gameDate.day, "hour": $gameDate.hour,
      "minutes": $gameDate.minutes}.toXmlAttributes
  saveTree.add(dateElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving map...", debugType = everything)
  for x in MapXRange.low .. MapXRange.high:
    for y in MapYRange.low .. MapYRange.high:
      if skyMap[x][y].visited:
        var fieldElement = newElement("field")
        fieldElement.attrs = {"x": $x, "y": $y}.toXmlAttributes
        saveTree.add(fieldElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving bases...", debugType = everything)
  saveBases(saveTree)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving player ship...", debugType = everything)
  savePlayerShip(saveTree)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving known recipes...", debugType = everything)
  for recipe in knownRecipes:
    var recipeElement = newElement("recipe")
    recipeElement.attrs = {"index": recipe}.toXmlAttributes
    saveTree.add(recipeElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving messages...", debugType = everything)
  let messagesToSave = (if gameSettings.savedMessages > messagesAmount(): messagesAmount() else: gameSettings.savedMessages)
  for i in (messagesAmount() - messagesToSave + 1) .. messagesAmount():
    let message = getMessage(i)
    var messageElement = newElement("message")
    messageElement.attrs = {"type": $message.kind,
        "color": $message.color}.toXmlAttributes
    messageElement.add(newText($message.message))
    saveTree.add(messageElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving events...", debugType = everything)
  for event in eventsList:
    var
      eventElement = newElement("event")
      eventData: string
    case event.eType
    of doublePrice:
      eventData = $event.itemIndex
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      eventData = $event.shipIndex
    else:
      eventData = $event.data
    eventElement.attrs = {"data": eventData, "type": $event.eType.ord,
        "x": $event.skyX, "y": $event.skyY, "time": $event.time}.toXmlAttributes
    saveTree.add(eventElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving game statistics...", debugType = everything)
  var statsElement = newElement("statistics")
  statsElement.attrs = {"visitedbases": $gameStats.basesVisited,
      "mapdiscovered": $gameStats.mapVisited,
      "distancetraveled": $gameStats.distanceTraveled,
      "acceptedmissions": $gameStats.acceptedMissions,
      "points": $gameStats.points}.toXmlAttributes
  proc saveStatistics(stats: seq[StatisticsData], statName: string) =
    for statistic in stats:
      var statElement = newElement(statName)
      statElement.attrs = {"index": statistic.index,
          "amount": $statistic.amount}.toXmlAttributes
      statsElement.add(statElement)
  saveStatistics(gameStats.destroyedShips, "destroyedships")
  saveStatistics(gameStats.craftingOrders, "finishedcrafts")
  saveStatistics(gameStats.finishedMissions, "finishedmissions")
  saveStatistics(gameStats.finishedGoals, "finishedgoals")
  saveStatistics(gameStats.killedMobs, "killedmobs")
  saveTree.add(statsElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving current goal...", debugType = everything)
  var goalElement = newElement("currentgoal")
  goalElement.attrs = {"index": $currentGoal.index,
      "type": $currentGoal.goalType.ord, "amount": $currentGoal.amount,
      "target": currentGoal.targetIndex,
      "multiplier": $currentGoal.multiplier}.toXmlAttributes
  saveTree.add(goalElement)
  logMessage(message = "done", debugType = everything)
  if currentStory.index.len > 0:
    logMessage(message = "Saving current story...", debugType = everything)
    var
      storyElement = newElement("currentstory")
      attrs: seq[tuple[key, val: string]] = @[]
    attrs.add(("index", currentStory.index))
    case currentStory.currentStep
    of 0:
      attrs.add(("currentstep", "start"))
    of -1:
      attrs.add(("currentstep", "finish"))
    else:
      attrs.add(("currentstep", storiesList[currentStory.index].steps[
          currentStory.currentStep].index))
    attrs.add(("maxsteps", $currentStory.maxSteps))
    if currentStory.showText:
      attrs.add(("showText", "Y"))
    else:
      attrs.add(("showText", "N"))
    if currentStory.data.len > 0:
      attrs.add(("data", currentStory.data))
    attrs.add(("finishedstep", $currentStory.finishedStep.ord))
    storyElement.attrs = attrs.toXmlAttributes
    saveTree.add(storyElement)
    logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving finished stories...", debugType = everything)
  for finishedStory in finishedStories.items:
    var
      storyNode = newXmlTree("finishedstory", [], {
        "index": finishedStory.index,
        "stepsamount": $finishedStory.stepsAmount}.toXmlAttributes)
    for text in finishedStory.stepsTexts.items:
      var textElement = newElement("steptext")
      textElement.add(newText(text))
      storyNode.add(textElement)
    saveTree.add(storyNode)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving accepted missions...", debugType = everything)
  for mission in acceptedMissions:
    var
      missionElement = newElement("acceptedmission")
      attrs: seq[tuple[key, val: string]] = @[]
    attrs.add(("type", $mission.mType.ord))
    case mission.mType
    of deliver:
      attrs.add(("target", $mission.itemIndex))
    of passenger:
      attrs.add(("target", $mission.data))
    of destroy:
      attrs.add(("target", $mission.shipIndex))
    else:
      attrs.add(("target", $mission.target))
    attrs.add(("time", $mission.time))
    attrs.add(("targetx", $mission.targetX))
    attrs.add(("targety", $mission.targetY))
    attrs.add(("reward", $mission.reward))
    attrs.add(("startbase", $mission.startBase))
    if mission.finished:
      attrs.add(("finished", "Y"))
    else:
      attrs.add(("finished", "N"))
    if mission.multiplier != 1.0:
      attrs.add(("multiplier", $mission.multiplier))
    missionElement.attrs = attrs.toXmlAttributes()
    saveTree.add(missionElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving player career...", debugType = everything)
  var careerElement = newElement("playercareer")
  careerElement.attrs = {"index": playerCareer}.toXmlAttributes()
  saveTree.add(careerElement)
  logMessage(message = "done", debugType = everything)
  var saveText = $saveTree
  if not prettyPrint:
    var lines = saveText.splitLines
    for line in lines.mitems:
      line = line.strip
    saveText = lines.join
  writeFile(saveName, saveText)
  logMessage(message = "Finished saving game in file " & saveName & ".",
      debugType = everything)

proc loadGame*() =
  let savedGame = loadXml(saveName)
  logMessage(message = "Start loading game from file " & saveName & ".",
      debugType = everything)
  # Check the same game compatybility
  if savedGame.attr("version").parseInt > saveVersion:
    raise newException(exceptn = DataLoadingError,
        message = "This save is incompatible with this version of the game.")
  # Load the game difficulty settings
  logMessage(message = "Loading the game difficulty settings...",
      debugType = everything)
  var diffNode = savedGame.child("difficulty")
  newGameSettings.enemyDamageBonus = diffNode.attr(
      "enemydamagebonus").parseFloat
  newGameSettings.playerDamageBonus = diffNode.attr(
      "playerdamagebonus").parseFloat
  newGameSettings.enemyMeleeDamageBonus = diffNode.attr(
      "enemymeleedamagebonus").parseFloat
  newGameSettings.playerMeleeDamageBonus = diffNode.attr(
      "playermeleedamagebonus").parseFloat
  newGameSettings.experienceBonus = diffNode.attr("experiencebonus").parseFloat
  newGameSettings.reputationBonus = diffNode.attr("reputationbonus").parseFloat
  newGameSettings.upgradeCostBonus = diffNode.attr(
      "upgradecostbonus").parseFloat
  newGameSettings.pricesBonus = diffNode.attr("pricesbonus").parseFloat
  logMessage(message = "done", debugType = everything)
  # Load the game date
  logMessage(message = "Loading the game time...", debugType = everything)
  var dateNode = savedGame.child("gamedate")
  gameDate.year = dateNode.attr("year").parseInt
  gameDate.month = dateNode.attr("month").parseInt
  gameDate.day = dateNode.attr("day").parseInt
  gameDate.hour = dateNode.attr("hour").parseInt
  gameDate.minutes = dateNode.attr("minutes").parseInt
  logMessage(message = "done", debugType = everything)
  # Load the sky map
  logMessage(message = "Loading the map...", debugType = everything)
  for x in 1 .. 1_024:
    for y in 1 .. 1_024:
      skyMap[x][y].missionIndex = -1
      skyMap[x][y].baseIndex = 0
      skyMap[x][y].eventIndex = -1
      skyMap[x][y].visited = false
  for field in savedGame.findAll("field"):
    let
      x = field.attr("x").parseInt
      y = field.attr("y").parseInt
    skyMap[x][y].visited = true
  logMessage(message = "done", debugType = everything)
  # Load sky bases
  logMessage(message = "Loading bases...", debugType = everything)
  loadBases(saveData = savedGame)
  logMessage(message = "done", debugType = everything)
  # Load the player's ship
  logMessage(message = "Loading the player's ship...", debugType = everything)
  loadPlayerShip(saveData = savedGame)
  logMessage(message = "done", debugType = everything)
  # Load known recipes
  logMessage(message = "Loading known recipes...", debugType = everything)
  for recipe in savedGame.findAll("recipe"):
    knownRecipes.add(recipe.attr("index"))
  logMessage(message = "done", debugType = everything)
  # Load messages
  logMessage(message = "Loading messages...", debugType = everything)
  for message in savedGame.findAll("message"):
    restoreMessage(message = message.innerText, kind = parseEnum[MessageType](
        s = message.attr("type")), color = parseEnum[MessageColor](
        s = message.attr("color")))
  logMessage(message = "done", debugType = everything)
  # Load events
  logMessage(message = "Loading events...", debugType = everything)
  for index, savedEvent in savedGame.findAll("event"):
    var event = EventData(skyX: savedEvent.attr("x").parseInt,
        skyY: savedEvent.attr("y").parseInt, time: savedEvent.attr(
        "time").parseInt, eType: savedEvent.attr("type").parseInt.EventsTypes)
    case event.eType
      of doublePrice:
        event.itemIndex = savedEvent.attr("data").parseInt
      of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
        event.shipIndex = savedEvent.attr("data").parseInt
      else:
        event.data = savedEvent.attr("data").parseInt
    eventsList.add(event)
    skyMap[event.skyX][event.skyY].eventIndex = index + 1
  logMessage(message = "done", debugType = everything)
  # Load the current story
  let storyNode = savedGame.child("currentstory")
  if storyNode != nil:
    logMessage(message = "Loading the current story...", debugType = everything)
    currentStory.index = storyNode.attr("index")
    currentStory.step = storyNode.attr("step").parseInt
    case storyNode.attr("currentstep")
    of "start":
      currentStory.currentStep = 0
    of "finish":
      currentStory.currentStep = -1
    else:
      for index, step in storiesList[currentStory.index].steps:
        if step.index == storyNode.attr("currentStep"):
          currentStory.currentStep = index
          break
    currentStory.maxSteps = storyNode.attr("maxsteps").parseInt
    currentStory.showText = storyNode.attr("showtext") == "Y"
    currentStory.finishedStep = storyNode.attr(
        "finishedstep").parseInt.StepConditionType
    logMessage(message = "done", debugType = everything)
  # Load finished stories data
  logMessage(message = "Loading finished stories...", debugType = everything)
  for savedStory in savedGame.findAll("finishedstory"):
    var story = FinishedStoryData()
    story.index = savedStory.attr("index")
    story.stepsAmount = savedStory.attr("stepsamount").parseInt
    for text in savedStory:
      story.stepsTexts.add(text.innerText)
    finishedStories.add(story)
  logMessage(message = "done", debugType = everything)
  # Load accepted missions
  logMessage(message = "Loading accepted missions...", debugType = everything)
  for index, mission in savedGame.findAll("acceptedmission"):
    var tmpMission = MissionData(mtype: mission.attr(
        "type").parseInt.MissionsTypes, time: mission.attr("time").parseInt,
        targetX: mission.attr("targetx").parseInt, targetY: mission.attr(
        "targety").parseInt, reward: mission.attr("reward").parseInt,
        startBase: mission.attr("startbase").parseInt, finished: mission.attr(
        "finished") == "Y", multiplier: 1.0)
    case tmpMission.mType
    of deliver:
      tmpMission.itemIndex = mission.attr("target").parseInt
    of passenger:
      tmpMission.data = mission.attr("target").parseInt
    of destroy:
      tmpMission.shipIndex = mission.attr("target").parseInt
    else:
      tmpMission.target = mission.attr("target").parseInt
    let multiplier = mission.attr("multiplier")
    if multiplier.len > 0:
      tmpMission.multiplier = multiplier.parseFloat
    acceptedMissions.add(tmpMission)
    skyMap[tmpMission.targetX][tmpMission.targetY].missionIndex = index
  logMessage(message = "done", debugType = everything)
  # Load game statistics
  logMessage(message = "Loading game statistics...", debugType = everything)
  for stat in savedGame.findAll("statistics"):
    gameStats.basesVisited = stat.attr("visitedbases").parseInt
    gameStats.mapVisited = stat.attr("mapdiscovered").parseInt
    gameStats.distanceTraveled = stat.attr("distancetraveled").parseInt
    gameStats.acceptedMissions = stat.attr("acceptedmissions").parseInt
    gameStats.points = stat.attr("points").parseInt
  for item in savedGame.findAll("destroyedships"):
    gameStats.destroyedShips.add(StatisticsData(index: item.attr("index"),
        amount: item.attr("amount").parseInt))
  for item in savedGame.findAll("finishedcrafts"):
    gameStats.craftingOrders.add(StatisticsData(index: item.attr("index"),
        amount: item.attr("amount").parseInt))
  for item in savedGame.findAll("finishedmissions"):
    gameStats.finishedMissions.add(StatisticsData(index: item.attr("index"),
        amount: item.attr("amount").parseInt))
  for item in savedGame.findAll("finishedgoals"):
    gameStats.finishedGoals.add(StatisticsData(index: item.attr("index"),
        amount: item.attr("amount").parseInt))
  for item in savedGame.findAll("killedmobs"):
    gameStats.killedMobs.add(StatisticsData(index: item.attr("index"),
        amount: item.attr("amount").parseInt))
  logMessage(message = "done", debugType = everything)
  # Load the player's current goal
  logMessage(message = "Loading game current goal...", debugType = everything)
  var goalNode = savedGame.child("currentgoal")
  currentGoal = GoalData(index: goalNode.attr("index"), goalType: goalNode.attr(
      "type").parseInt.GoalTypes, amount: goalNode.attr("amount").parseInt,
      targetIndex: goalNode.attr("target"), multiplier: goalNode.attr(
      "multiplier").parseInt)
  logMessage(message = "done", debugType = everything)
  # Load the player's career
  logMessage(message = "Loading the player's career...", debugType = everything)
  let careerNode = savedGame.child("playercareer")
  playerCareer = careerNode.attr("index")
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Finished loading the game.", debugType = everything)

proc generateSaveName*(renameSave: bool = false) {.sideEffect, raises: [OSError,
    IOError, Exception], tags: [ReadDirEffect, WriteIOEffect, ReadIOEffect].} =
  ## Generate an unique name for the save game file
  ##
  ## * renameSave - if true, rename the existing save game file.
  let oldSaveName = saveName
  while true:
    saveName = saveDirectory & playerShip.crew[0].name & "_" & playerShip.name &
        "_" & $getRandom(min = 100, max = 999) & ".sav"
    if not fileExists(saveName):
      break
  if renameSave:
    if fileExists(oldSaveName):
      moveFile(oldSaveName, saveName)

# Temporary code for interfacing with Ada

proc getAdaSaveName(name: cstring) {.raises: [], tags: [], exportc.} =
  saveName = $name

proc setAdaSaveName(name: var cstring) {.raises: [], tags: [], exportc.} =
  name = saveName.cstring

proc saveAdaGame(prettyPrint: cint) {.raises: [], tags: [WriteIOEffect,
    RootEffect], exportc.} =
  try:
    saveGame(prettyPrint = prettyPrint == 1)
  except KeyError, IOError:
    discard

proc loadAdaGame() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc.} =
  try:
    loadGame()
  except XmlError, ValueError, IOError, OSError, Exception:
    discard

proc generateAdaSaveName(renameSave: cint) {.raises: [], tags: [WriteIOEffect,
    ReadIOEffect], exportc.} =
  try:
    generateSaveName(renameSave = renameSave == 1)
  except OSError, IOError, Exception:
    discard
