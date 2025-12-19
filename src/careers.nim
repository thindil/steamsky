# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to the player's careers, like loading them from files.

import std/[logging, paths, strutils, tables, xmlparser, xmltree]
import contracts
import game, log

type
  CareerName* = string
    ## The name of a career
  CareerData = object
    ## Used to store data about available player's careers
    ##
    ## * mame   - The name of the career
    ## * skills - The list of skills which have bonuses from the career
    name: CareerName
    skills: seq[SkillName]

proc name*(career: CareerData): CareerName {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of CareerData type
  ##
  ## * career - the CommandLists object which field will be get
  ##
  ## Returns the value of the selected field
  career.name

proc skills*(career: CareerData): seq[SkillName] {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of CareerData type
  ##
  ## * career - the CommandLists object which field will be get
  ##
  ## Returns the value of the selected field
  career.skills

var careersList*: Table[string, CareerData] = initTable[string, CareerData]()
  ## The list of available player's careers in the game

proc loadCareers*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the player's careers' data from the file
  ##
  ## * fileName - the path to the file with careers data which will be loaded
  require:
    ($fileName).len > 0
  body:
    let careersXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load careers data file. Reason: " &
            getCurrentExceptionMsg())
    for careerNode in careersXml:
      if careerNode.kind != xnElement:
        continue
      let
        careerIndex: string = careerNode.attr(name = "index")
        careerAction: DataAction = try:
            parseEnum[DataAction](s = careerNode.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if careerAction in [update, remove]:
        if careersList.hasKey(key = careerIndex):
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $careerAction & " career '" & careerIndex & "', there is no career with that index,")
      elif careersList.hasKey(key = careerIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't add career '" & careerIndex & "', there is a career with that index.")
      if careerAction == DataAction.remove:
        careersList.del(key = careerIndex)
        logMessage(message = "Career removed: '" & careerIndex & "'",
            messageLevel = lvlInfo)
        continue
      var career: CareerData = if careerAction == DataAction.update:
          try:
            careersList[careerIndex]
          except KeyError:
            CareerData()
        else:
          CareerData()
      var attribute: string = careerNode.attr(name = "name")
      if attribute.len() > 0:
        career.name = attribute
      for skillNode in careerNode:
        if skillNode.kind != xnElement:
          continue
        let skillName: string = skillNode.attr(name = "name")
        if skillNode.attr(name = "action") == "remove":
          for index, skill in career.skills:
            if skill == skillName:
              career.skills.delete(i = index)
              break
        else:
          if findSkillIndex(skillName = skillName) == 0:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $careerAction & " career '" &
                careerIndex &
                "', no skill with name '" & skillName & "'.")
          career.skills.add(y = skillName)
      if careerAction == DataAction.add:
        logMessage(message = "Career added: '" & careerIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Career updated: '" & careerIndex & "'",
            messageLevel = lvlInfo)
      careersList[careerIndex] = career
