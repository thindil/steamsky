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

import std/[strutils, tables, xmltree]
import basessaveload, config, game, goals, log, maps, messages, missions,
    shipssaveload, statistics, stories, types

const saveVersion = 5 ## The current version of the game saves files

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
  let messagesToSave = (if gameSettings.savedMessages > messagesAmount(
      0): messagesAmount(0) else: gameSettings.savedMessages)
  for i in (messagesAmount(0) - messagesToSave + 1) .. messagesAmount(0):
    let message = getMessage(i, 0)
    var messageElement = newElement("message")
    messageElement.attrs = {"type": $message.kind,
        "color": $message.color}.toXmlAttributes
    messageElement.add(newText($message.message))
    saveTree.add(messageElement)
  logMessage(message = "done", debugType = everything)
  logMessage(message = "Saving events...", debugType = everything)
  for event in eventsList.values:
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
  saveStatistics(gameStats.destroyedShips, "destroyedShips")
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

# Temporary code for interfacing with Ada

proc getAdaSaveName(name: cstring) {.raises: [], tags: [], exportc.} =
  saveName = $name

proc saveAdaGame(prettyPrint: cint) {.raises: [], tags: [WriteIOEffect,
    RootEffect], exportc.} =
  try:
    saveGame(prettyPrint = prettyPrint == 1)
  except KeyError, IOError:
    discard