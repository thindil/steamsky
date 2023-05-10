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

import std/xmltree
import basessaveload, config, game, log, messages, shipssaveload, types

const saveVersion = 5

var saveName*: string

proc saveGame*(prettyPrint: bool = false) =
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
  let messagesToSave = (if gameSettings.savedMessages > messagesAmount(0): messagesAmount(0) else: gameSettings.savedMessages)
  for i in (messagesAmount(0) - messagesToSave + 1) .. messagesAmount(0):
    let
      message = getMessage(i, 0)
      kind = message.kind.ord
    var messageElement = newElement("message")
    messageElement.attrs = {"type": $message.kind, "color": $message}.toXmlAttributes
