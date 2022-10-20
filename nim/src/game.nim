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

import std/[os, tables, xmlparser, xmltree]

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

var
  saveDirectory*: string = "data" & DirSep & "saves" &
      DirSep ## The directory where the saved games and logs are stored
  moneyIndex*: Positive ## The item's index of the item used as money in the game
  moneyName*: string ## The name of the item used as a money in the game
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

proc findSkillIndex*(skillName: string): Natural =
  for key, skill in skillsList.pairs:
    if skill.name == skillName:
      return key
  return 0

proc loadData(fileName: string) =
  let gameXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load game data file. Reason: " &
          getCurrentExceptionMsg())
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


# Temporary code for interfacing with Ada

proc loadAdaData(fileName: cstring) {.exportc.} =
  loadData(fileName = $fileName)

proc getAdaListValue(listIndex, itemIndex: cint): cstring {.exportc.} =
  case listIndex
  of 0:
    if itemIndex >= basesSyllablesPreList.len():
      return ""
    return basesSyllablesPreList[itemIndex].cstring
  of 1:
    if itemIndex >= basesSyllablesStartList.len():
      return ""
    return basesSyllablesStartList[itemIndex].cstring
  of 2:
    if itemIndex >= basesSyllablesEndList.len():
      return ""
    return basesSyllablesEndList[itemIndex].cstring
  of 3:
    if itemIndex >= basesSyllablesPostList.len():
      return ""
    return basesSyllablesPostList[itemIndex].cstring
  else:
    return ""
