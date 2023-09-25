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

import std/[strutils, xmltree]
import game, types

proc saveBases*(saveData: var XmlNode) {.sideEffect, raises: [], tags: [].} =
  ## Save the bases from the current game into a file
  ##
  ## * saveData - the XML structure to which the bases will be saved
  for skyBase in skyBases.items:
    let
      askedForBases = (if skyBase.askedForBases: "Y" else: "N")
      knownBase = (if skyBase.known: "Y" else: "N")
    var baseTree = newXmlTree("base", [], {"name": skyBase.name,
        "type": $skyBase.baseType, "population": $skyBase.population,
        "x": $skyBase.skyX, "y": $skyBase.skyY, "askedforbases": askedForBases,
        "known": knownBase, "owner": skyBase.owner.toUpperAscii(),
        "size": toUpperAscii( $skyBase.size)}.toXmlAttributes)
    if skyBase.visited.year > 0:
      var saveDate = newElement("visiteddate")
      saveDate.attrs = {"year": $skyBase.visited.year,
          "month": $skyBase.visited.month, "day": $skyBase.visited.day,
          "hour": $skyBase.visited.hour,
          "minutes": $skyBase.visited.minutes}.toXmlAttributes
      baseTree.add(saveDate)
      saveDate = newElement("recruitdate")
      saveDate.attrs = {"year": $skyBase.recruitDate.year,
          "month": $skyBase.recruitDate.month,
          "day": $skyBase.recruitDate.day}.toXmlAttributes
      baseTree.add(saveDate)
      if skyBase.reputation.level != 0:
        var repElement = newElement("reputation")
        if skyBase.reputation.experience > 0:
          repElement.attrs = {"level": $skyBase.reputation.level,
              "progress": $skyBase.reputation.experience}.toXmlAttributes
        else:
          repElement.attrs = {"level": $skyBase.reputation.level}.toXmlAttributes
        baseTree.add(repElement)
      for recruit in skyBase.recruits:
        var recruitNode = newXmlTree("recruit", [], {"name": recruit.name,
            "gender": $recruit.gender, "price": $recruit.price,
            "payment": $recruit.payment, "homebase": $recruit.homeBase,
            "faction": recruit.faction}.toXmlAttributes)
        for skill in recruit.skills:
          var skillElement = newElement("skill")
          skillElement.attrs = {"index": $skill.index,
              "level": $skill.level}.toXmlAttributes
          recruitNode.add(skillElement)
        for attribute in recruit.attributes:
          var attrElement = newElement("attribute")
          attrElement.attrs = {"level": $attribute.level}.toXmlAttributes
          recruitNode.add(attrElement)
        for item in recruit.inventory:
          var itemElement = newElement("item")
          itemElement.attrs = {"index": $item}.toXmlAttributes
          recruitNode.add(itemElement)
        for index, item in recruit.equipment.pairs:
          if item > -1:
            var itemElement = newElement("equipment")
            itemElement.attrs = {"slot": $(index.int + 1), "index": $(item +
                1)}.toXmlAttributes
            recruitNode.add(itemElement)
        baseTree.add(recruitNode)
      saveDate = newElement("askedforeventsdate")
      saveDate.attrs = {"year": $skyBase.askedForEvents.year,
          "month": $skyBase.askedForEvents.month,
          "day": $skyBase.askedForEvents.day}.toXmlAttributes
      baseTree.add(saveDate)
      saveDate = newElement("missionsdate")
      saveDate.attrs = {"year": $skyBase.missionsDate.year,
          "month": $skyBase.missionsDate.month,
          "day": $skyBase.missionsDate.day}.toXmlAttributes
      baseTree.add(saveDate)
      for mission in skyBase.missions:
        var
          missionElement = newElement("mission")
          target: string = case mission.mType
            of deliver:
              $mission.itemIndex
            of passenger:
              $mission.data
            of destroy:
              $mission.shipIndex
            else:
              $mission.target
        missionElement.attrs = {"type": $mission.mType.ord, "target": target,
            "time": $mission.time, "targetx": $mission.targetX,
            "targety": $mission.targetY,
            "reward": $mission.reward}.toXmlAttributes
        baseTree.add(missionElement)
    if skyBase.reputation.level != 0:
      var repElement = newElement("reputation")
      if skyBase.reputation.experience > 0:
        repElement.attrs = {"level": $skyBase.reputation.level,
            "progress": $skyBase.reputation.experience}.toXmlAttributes
      else:
        repElement.attrs = {"level": $skyBase.reputation.level}.toXmlAttributes
      baseTree.add(repElement)
    for item in skyBase.cargo:
      var itemElement = newElement("item")
      itemElement.attrs = {"index": $item.protoIndex, "amount": $item.amount,
          "durability": $item.durability, "price": $item.price}.toXmlAttributes
      baseTree.add(itemElement)
    saveData.add(baseTree)

proc loadBases*(saveData: var XmlNode) =
  var baseIndex = 1
  for base in saveData.findAll("base"):
    skyBases[baseIndex].name = base.attr("name")
    skyBases[baseIndex].visited = DateRecord()
    skyBases[baseIndex].skyX = base.attr("x").parseInt
    skyBases[baseIndex].skyY = base.attr("y").parseInt
    skyBases[baseIndex].baseType = base.attr("type")
    skyBases[baseIndex].population = base.attr("population").parseInt
    skyBases[baseIndex].recruitDate = DateRecord()
    skyBases[baseIndex].recruits = @[]
    skyBases[baseIndex].known = false
    skyBases[baseIndex].askedForBases = false
    skyBases[baseIndex].askedForEvents = DateRecord()
    skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)
    skyBases[baseIndex].missionsDate = DateRecord()
    skyBases[baseIndex].missions = @[]
    skyBases[baseIndex].owner = base.attr("owner")
    skyBases[baseIndex].size = parseEnum[BasesSize](base.attr("size"))
    if base.attr("askedforbases") == "Y":
      skyBases[baseIndex].askedForBases = true
    let visitDate = base.child("visiteddate")
    if visitDate != nil:
      skyBases[baseIndex].visited = DateRecord(year: visitDate.attr(
          "year").parseInt, month: visitDate.attr("month").parseInt,
          day: visitDate.attr("day").parseInt, hour: visitDate.attr(
          "hour").parseInt, minutes: visitDate.attr("minutes").parseInt)
    let recruitDate = base.child("recruitdate")
    if recruitDate != nil:
      skyBases[baseIndex].recruitDate = DateRecord(year: recruitDate.attr(
          "year").parseInt, month: recruitDate.attr("month").parseInt,
          day: recruitDate.attr("day").parseInt, hour: recruitDate.attr(
          "hour").parseInt, minutes: recruitDate.attr("minutes").parseInt)
    let askDate = base.child("askedforeventsdate")
    if recruitDate != nil:
      skyBases[baseIndex].askedForEvents = DateRecord(year: askDate.attr(
          "year").parseInt, month: askDate.attr("month").parseInt,
          day: askDate.attr("day").parseInt, hour: askDate.attr(
          "hour").parseInt, minutes: askDate.attr("minutes").parseInt)
    for baseRecruit in base.findAll("recruit"):
      var
        recruit = RecruitData()
      recruit.name = baseRecruit.attr("name")
      recruit.gender = baseRecruit.attr("gender")[0]
      recruit.price = baseRecruit.attr("price").parseInt
      for recruitSkill in baseRecruit.findAll("skill"):
        var skill = SkillInfo()
        skill.index = recruitSkill.attr("index").parseInt
        skill.level = recruitSkill.attr("level").parseInt
        recruit.skills.add(skill)
      for recruitAttr in baseRecruit.findAll("attribute"):
        recruit.attributes.add(MobAttributeRecord(level: recruitAttr.attr(
            "level").parseInt, experience: 0))

