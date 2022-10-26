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

{.used.}

import std/[strutils, tables, xmlparser, xmltree]
import game, log

type
  CareerData = object
    name*: string
    skills: seq[string]

var careersList*: Table[string, CareerData] = initTable[string, CareerData]()

proc loadCareers*(fileName: string) =
  let careersXml = loadXml(path = fileName)
  for careerNode in careersXml:
    if careerNode.kind != xnElement:
      continue
    let
      careerIndex = careerNode.attr(name = "index")
      careerAction: DataAction = try:
          parseEnum[DataAction](careerNode.attr(name = "action").toLowerAscii)
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
      logMessage(message = "career removed: '" & careerIndex & "'",
          debugType = everything)
      continue
    var career: CareerData = if careerAction == DataAction.update:
        careersList[careerIndex]
      else:
        CareerData()
    var attribute = careerNode.attr(name = "name")
    if attribute.len() > 0:
      career.name = attribute
    for skillNode in careerNode:
      if skillNode.kind != xnElement:
        continue
      let skillName = skillNode.attr(name = "name")
      if skillNode.attr(name = "action") == "remove":
        for index, skill in career.skills.pairs:
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
    careersList[careerIndex] = career

proc loadAdaCareers*(fileName: cstring) {.exportc.} =
  loadCareers(fileName = $fileName)

proc getAdaCareer(index: cint; adaCareer: var array[2, cstring]) {.sideEffect,
    raises: [], tags: [], exportc.} =
  adaCareer = ["".cstring, "".cstring]
  if index > careersList.len():
    return
  var
    career: CareerData
    careerIndex: Positive = 1
  for key in careersList.keys:
    if careerIndex == index:
      try:
        career = careersList[key]
        adaCareer[0] = key.cstring
      except KeyError:
        return
      break
    careerIndex.inc()
  adaCareer[1] = career.name.cstring

proc getAdaCareerSkill(careerIndex: cstring; index: cint): cstring {.exportc.} =
  if index >= careersList[$careerIndex].skills.len():
    return ""
  return careersList[$careerIndex].skills[index].cstring

