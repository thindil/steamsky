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
import game, maps, types

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

proc loadBases*(saveData: XmlNode) {.sideEffect, raises: [ValueError], tags: [].} =
  ## Load the bases from the file into the game
  ##
  ## * saveData - the XML structure from which the bases will be loaded
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
    skyBases[baseIndex].size = parseEnum[BasesSize](base.attr("size").toLowerAscii)
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
          day: recruitDate.attr("day").parseInt, hour: 0, minutes: 0)
    let askDate = base.child("askedforeventsdate")
    if askDate != nil:
      skyBases[baseIndex].askedForEvents = DateRecord(year: askDate.attr(
          "year").parseInt, month: askDate.attr("month").parseInt,
          day: askDate.attr("day").parseInt, hour: 0, minutes: 0)
    for baseRecruit in base.findAll("recruit"):
      var recruit = RecruitData()
      recruit.name = baseRecruit.attr("name")
      recruit.gender = baseRecruit.attr("gender")[0]
      recruit.price = baseRecruit.attr("price").parseInt
      recruit.payment = 20
      recruit.homeBase = 1
      for recruitSkill in baseRecruit.findAll("skill"):
        var skill = SkillInfo()
        skill.index = recruitSkill.attr("index").parseInt
        skill.level = recruitSkill.attr("level").parseInt
        recruit.skills.add(skill)
      for recruitAttr in baseRecruit.findAll("attribute"):
        recruit.attributes.add(MobAttributeRecord(level: recruitAttr.attr(
            "level").parseInt, experience: 0))
      for item in baseRecruit.findAll("item"):
        recruit.inventory.add(item.attr("index").parseInt)
      for equipment in baseRecruit.findAll("equipment"):
        var eqIndex = (equipment.attr("slot").parseInt - 1)
        recruit.equipment[eqIndex.EquipmentLocations] = equipment.attr(
            "index").parseInt
      let payment = baseRecruit.child("payment")
      if payment != nil:
        recruit.payment = payment.attr("payment").parseInt
      let homeBase = baseRecruit.child("homebase")
      if homeBase != nil:
        recruit.homeBase = homeBase.attr("homebase").parseInt
      let faction = baseRecruit.child("faction")
      if faction != nil:
        recruit.faction = faction.attr("faction")
      skyBases[baseIndex].recruits.add(recruit)
    let reputation = base.child("reputation")
    if reputation != nil:
      skyBases[baseIndex].reputation.level = reputation.attr("level").parseInt
      let progress = reputation.attr("progress")
      if progress.len > 0:
        skyBases[baseIndex].reputation.experience = progress.parseInt
    let missionsDate = base.child("missionsdate")
    if missionsDate != nil:
      skyBases[baseIndex].missionsDate = DateRecord(year: missionsDate.attr(
          "year").parseInt, month: missionsDate.attr("month").parseInt,
          day: missionsDate.attr("day").parseInt, hour: 0, minutes: 0)
    for mission in base.findAll("mission"):
      let mType = mission.attr("type").parseInt.MissionsTypes
      var
        targetIndex: string
        target: int
      if mType in {deliver, destroy}:
        targetIndex = mission.attr("target")
      else:
        target = mission.attr("target").parseInt
      let
        time = mission.attr("time").parseInt
        targetX = mission.attr("targetx").parseInt
        targetY = mission.attr("targety").parseInt
        reward = mission.attr("reward").parseInt
      case mType
      of deliver:
        skyBases[baseIndex].missions.add(MissionData(mType: deliver,
            itemIndex: targetIndex.parseInt, time: time, targetX: targetX,
            targetY: targetY, reward: reward, startBase: baseIndex,
            finished: false, multiplier: 1.0))
      of destroy:
        skyBases[baseIndex].missions.add(MissionData(mType: destroy,
            shipIndex: targetIndex.parseInt, time: time, targetX: targetX,
            targetY: targetY, reward: reward, startBase: baseIndex,
            finished: false, multiplier: 1.0))
      of patrol:
        skyBases[baseIndex].missions.add(MissionData(mType: patrol,
            target: target, time: time, targetX: targetX, targetY: targetY,
            reward: reward, startBase: baseIndex, finished: false,
            multiplier: 1.0))
      of explore:
        skyBases[baseIndex].missions.add(MissionData(mType: explore,
            target: target, time: time, targetX: targetX, targetY: targetY,
            reward: reward, startBase: baseIndex, finished: false,
            multiplier: 1.0))
      of passenger:
        if target > 91:
          target = 91
        skyBases[baseIndex].missions.add(MissionData(mType: passenger,
            data: target, time: time, targetX: targetX, targetY: targetY,
            reward: reward, startBase: baseIndex, finished: false,
            multiplier: 1.0))
    for baseItem in base.findAll("item"):
      if baseItem.attr("durability").len == 0:
        continue
      var item = BaseCargo()
      item.protoIndex = baseItem.attr("index").parseInt
      item.durability = baseItem.attr("durability").parseInt
      item.amount = baseItem.attr("amount").parseInt
      item.price = baseItem.attr("price").parseInt
      skyBases[baseIndex].cargo.add(item)
    skyMap[skyBases[baseIndex].skyX][skyBases[
        baseIndex].skyY].baseIndex = baseIndex
    baseIndex.inc
