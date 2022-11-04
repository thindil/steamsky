# Copyright 2022 Bartek thindil Jasicki
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

import std/[os, strutils, tables, xmlparser, xmltree]

type
  DateRecord* = object
    ## FUNCTION
    ##
    ## Used to store the game's time
    year*: range[0..4_000_000] ## The game's year
    month*: range[0..24] ## The game's month
    day*: range[0..62] ## The game's day
    hour*: range[0..48] ## The game's hour
    minutes*: range[0..120] ## The game's minutes

  DataAction* = enum
    # FUNCTION
    #
    # Possible actions to do when loading game data
    add, update, remove

  MapXRange* = range[1..1_024] ## The size of the game map in X axis
  MapYRange* = range[1..1_024] ## The size of the game map in Y axis
  ReputationRange* = range[-100..100] ## The range of possible reputation levels
  AttributesArray* = array[1 .. 2, Natural] ## 1 - Attribute level, 2 - Attribute experience

  ToolQuality = object
    level: Natural
    quality: Natural

  SkillRecord* = object
    name: string
    attribute: Positive
    description: string
    tool: string
    toolsQuality: seq[ToolQuality]

  DataLoadingError* = object of CatchableError
    ## FUNCTION
    ##
    ## Used to mark problems during loading the game data from files

  AttributeRecord* = object
    name: string
    description: string

var
  saveDirectory*: string = "data" & DirSep & "saves" &
      DirSep            ## The directory where the saved games and logs are stored
  moneyIndex*: Positive ## The item's index of the item used as money in the game
  moneyName*: string    ## The name of the item used as a money in the game
  skillsList* = initTable[Positive, SkillRecord]()
  basesSyllablesPreList*: seq[string]
  basesSyllablesStartList*: seq[string]
  basesSyllablesEndList*: seq[string]
  basesSyllablesPostList*: seq[string]
  malesSyllablesStartList*: seq[string]
  malesSyllablesMiddleList*: seq[string]
  malesSyllablesEndList*: seq[string]
  malesVocalsList*: seq[string]
  malesConsonantsList*: seq[string]
  femalesSyllablesStartList*: seq[string]
  femalesSyllablesMiddleList*: seq[string]
  femalesSyllablesEndList*: seq[string]
  femalesVocalsList*: seq[string]
  shipsSyllablesStartList*: seq[string]
  shipsSyllablesMiddleList*: seq[string]
  shipsSyllablesEndList*: seq[string]
  attributesList*: seq[AttributeRecord]
  itemsTypesList*: seq[string]
  repairTools*: string
  cleaningTools*: string
  alchemyTools*: string
  corpseIndex*: Positive
  missionItemsType*: string
  fuelType*: string
  tradersName*: string
  conditionIndex*: Positive
  strengthIndex*: Positive
  pilotingSkill*: Positive
  engineeringSkill*: Positive
  gunnerySkill*: Positive
  talkingSkill*: Positive
  perceptionSkill*: Positive
  headArmor*: string
  chestArmor*: string
  armsArmor*: string
  legsArmor*: string
  shieldType*: string
  weaponType*: string
  dodgeSkill*: Positive
  unarmedSkill*: Positive

proc findSkillIndex*(skillName: string): Natural =
  for key, skill in skillsList.pairs:
    if skill.name == skillName:
      return key
  return 0

proc loadData*(fileName: string) =

  proc findAttributeIndex(attributeName: string): Natural =
    for key, attribute in attributesList.pairs:
      if attribute.name == attributeName:
        return key + 1
    return 0

  let gameXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load game data file. Reason: " &
          getCurrentExceptionMsg())
  var skillIndex: Positive = 1
  for gameNode in gameXml:
    if gameNode.kind != xnElement:
      continue
    case gameNode.tag
    of "basessyllablepre":
      basesSyllablesPreList.add(y = gameNode.attr(name = "value"))
    of "basessyllablestart":
      basesSyllablesStartList.add(y = gameNode.attr(name = "value"))
    of "basessyllableend":
      basesSyllablesEndList.add(y = gameNode.attr(name = "value"))
    of "basessyllablepost":
      basesSyllablesPostList.add(y = gameNode.attr(name = "value"))
    of "malessyllablestart":
      malesSyllablesStartList.add(y = gameNode.attr(name = "value"))
    of "malessyllablemiddle":
      malesSyllablesMiddleList.add(y = gameNode.attr(name = "value"))
    of "malessyllableend":
      malesSyllablesEndList.add(y = gameNode.attr(name = "value"))
    of "malesvocal":
      malesVocalsList.add(y = gameNode.attr(name = "value"))
    of "malesconsonant":
      malesConsonantsList.add(y = gameNode.attr(name = "value"))
    of "femalessyllablestart":
      femalesSyllablesStartList.add(y = gameNode.attr(name = "value"))
    of "femalessyllablemiddle":
      femalesSyllablesMiddleList.add(y = gameNode.attr(name = "value"))
    of "femalessyllableend":
      femalesSyllablesEndList.add(y = gameNode.attr(name = "value"))
    of "femalesvocal":
      femalesVocalsList.add(y = gameNode.attr(name = "value"))
    of "shipssyllablestart":
      shipsSyllablesStartList.add(y = gameNode.attr(name = "value"))
    of "shipssyllablemiddle":
      shipsSyllablesMiddleList.add(y = gameNode.attr(name = "value"))
    of "shipssyllableend":
      shipsSyllablesEndList.add(y = gameNode.attr(name = "value"))
    of "attribute":
      attributesList.add(y = AttributeRecord(name: gameNode.attr(name = "name"),
          description: gameNode.innerText()))
    of "skill":
      var newSkill: SkillRecord = SkillRecord(attribute: 1)
      newSkill.name = gameNode.attr(name = "name")
      newSkill.tool = gameNode.attr(name = "tool")
      let attributeName = gameNode.attr(name = "attribute")
      for index, attribute in attributesList.pairs():
        if attribute.name == attributeName:
          newSkill.attribute = index + 1
          break
      for childNode in gameNode:
        if childNode.kind != xnElement:
          continue
        case childNode.tag
        of "description":
          newSkill.description = childNode.innerText()
        of "toolquality":
          newSkill.toolsQuality.add(y = ToolQuality(level: childNode.attr(
              name = "level").parseInt(), quality: childNode.attr(
              name = "quality").parseInt()))
      skillsList[skillIndex] = newSkill
      skillIndex.inc()
    of "itemtype":
      itemsTypesList.add(y = gameNode.attr(name = "value"))
    of "remove":
      case gameNode.attr(name = "name")
      of "skill":
        {.warning[ProveInit]: off.}
        {.warning[UnsafeDefault]: off.}
        skillsList.del(key = gameNode.attr(name = "value").parseInt())
        {.warning[ProveInit]: on.}
        {.warning[UnsafeDefault]: on.}
      of "attribute":
        attributesList.del(i = gameNode.attr(name = "value").parseInt() - 1)
      of "itemtype":
        itemsTypesList.del(i = gameNode.attr(name = "value").parseInt() - 1)
      else:
        discard
    of "repairtools":
      repairTools = gameNode.attr(name = "value")
    of "cleaningtools":
      cleaningTools = gameNode.attr(name = "value")
    of "alchemytools":
      alchemyTools = gameNode.attr(name = "value")
    of "corpseindex":
      corpseIndex = gameNode.attr(name = "value").parseInt()
    of "missionitemstype":
      missionItemsType = gameNode.attr(name = "value")
    of "fueltype":
      fuelType = gameNode.attr(name = "value")
    of "moneyindex":
      moneyIndex = gameNode.attr(name = "value").parseInt()
    of "tradersname":
      tradersName = gameNode.attr(name = "value")
    of "conditionname":
      conditionIndex = findAttributeIndex(attributeName = gameNode.attr(
          name = "value"))
    of "strengthname":
      strengthIndex = findAttributeIndex(attributeName = gameNode.attr(
          name = "value"))
    of "pilotingskill":
      pilotingSkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
    of "engineeringskill":
      engineeringSkill = findSkillIndex(skillName = gameNode.attr(
          name = "value"))
    of "gunneryskill":
      gunnerySkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
    of "talkingskill":
      talkingSkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
    of "perceptionskill":
      perceptionSkill = findSkillIndex(skillName = gameNode.attr(
          name = "value"))
    of "headarmor":
      headArmor = gameNode.attr(name = "value")
    of "chestarmor":
      chestArmor = gameNode.attr(name = "value")
    of "armsarmor":
      armsArmor = gameNode.attr(name = "value")
    of "legsarmor":
      legsArmor = gameNode.attr(name = "value")
    of "shieldtype":
      shieldType = gameNode.attr(name = "value")
    of "weapontype":
      weaponType = gameNode.attr(name = "value")

# Temporary code for interfacing with Ada

proc loadAdaData(fileName: cstring) {.exportc.} =
  loadData(fileName = $fileName)

proc getAdaListValue(listIndex, itemIndex: cint): cstring {.exportc.} =
  case listIndex
  of 0:
    if itemIndex >= itemsTypesList.len():
      return ""
    return itemsTypesList[itemIndex].cstring
  else:
    return ""

proc getAdaAttribute(itemIndex: cint; attribute: var array[2,
    cstring]) {.exportc.} =
  attribute = ["".cstring, "".cstring]
  if itemIndex >= attributesList.len():
    return
  attribute = [attributesList[itemIndex].name.cstring, attributesList[
      itemIndex].description.cstring]

proc getAdaSkillToolsAmount(skillIndex: cint): cint {.exportc.} =
  if not skillsList.contains(key = skillIndex):
    return 0
  return skillsList[skillIndex].toolsQuality.len().cint

type AdaSkillRecord = object
  name: cstring
  attribute: cint
  description: cstring
  tool: cstring

proc getAdaSkill(skillIndex: cint; skill: var AdaSkillRecord) {.exportc.} =
  skill = AdaSkillRecord(name: "".cstring, attribute: 0,
      description: "".cstring, tool: "".cstring)
  if not skillsList.contains(key = skillIndex):
    return
  skill.name = skillsList[skillIndex].name.cstring
  skill.attribute = skillsList[skillIndex].attribute.cint
  skill.description = skillsList[skillIndex].description.cstring
  skill.tool = skillsList[skillIndex].tool.cstring

proc getAdaSkillTools(skillIndex: cint; tools: var array[16, array[2,
    cint]]) {.exportc.} =
  tools[0] = [-1.cint, -1.cint]
  if not skillsList.contains(key = skillIndex):
    return
  var index = 0
  for toolQuality in skillsList[skillIndex].toolsQuality:
    tools[index] = [toolQuality.level.cint, toolQuality.quality.cint]
    index.inc()

proc findAdaSkillIndex(skillName: cstring): cint {.exportc.} =
  return findSkillIndex(skillName = $skillName).cint

proc getAdaGameStrings(values: var array[0..11, cstring]) {.exportc.} =
  values = [repairTools.cstring, cleaningTools.cstring, alchemyTools.cstring,
      missionItemsType.cstring, fuelType.cstring, tradersName.cstring,
      headArmor.cstring, chestArmor.cstring, armsArmor.cstring,
      legsArmor.cstring, shieldType.cstring, weaponType.cstring]

proc getAdaGameIntegers(values: var array[0..8, cint]) {.exportc.} =
  values = [corpseIndex.cint, moneyIndex.cint, conditionIndex.cint,
      strengthIndex.cint, pilotingSkill.cint, engineeringSkill.cint,
      gunnerySkill.cint, talkingSkill.cint, perceptionSkill.cint]
