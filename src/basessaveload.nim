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

## Provides code related to saving and loading the sky bases data from
## a file.

import std/[strutils, xmltree]
import contracts
import game, maps, types

proc saveBases*(saveData: var XmlNode) {.raises: [], tags: [],
    contractual.} =
  ## Save the bases from the current game into a file
  ##
  ## * saveData - the XML structure to which the bases will be saved
  require:
    saveData != nil
  body:
    for skyBase in skyBases:
      let
        askedForBases: string = (if skyBase.askedForBases: "Y" else: "N")
        knownBase: string = (if skyBase.known: "Y" else: "N")
      var baseTree: XmlNode = newXmlTree(tag = "base", children = [],
          attributes = {"name": skyBase.name, "type": $skyBase.baseType,
          "population": $skyBase.population,
          "x": $skyBase.skyX, "y": $skyBase.skyY,
          "askedforbases": askedForBases,
          "known": knownBase, "owner": skyBase.owner.toUpperAscii(),
          "size": toUpperAscii(s = $skyBase.size)}.toXmlAttributes)
      if skyBase.visited.year > 0:
        var saveDate: XmlNode = newElement(tag = "visiteddate")
        saveDate.attrs = {"year": $skyBase.visited.year,
            "month": $skyBase.visited.month, "day": $skyBase.visited.day,
            "hour": $skyBase.visited.hour,
            "minutes": $skyBase.visited.minutes}.toXmlAttributes
        baseTree.add(son = saveDate)
        saveDate = newElement(tag = "recruitdate")
        saveDate.attrs = {"year": $skyBase.recruitDate.year,
            "month": $skyBase.recruitDate.month,
            "day": $skyBase.recruitDate.day}.toXmlAttributes
        baseTree.add(son = saveDate)
        if skyBase.reputation.level != 0:
          var repElement: XmlNode = newElement(tag = "reputation")
          if skyBase.reputation.experience > 0:
            repElement.attrs = {"level": $skyBase.reputation.level,
                "progress": $skyBase.reputation.experience}.toXmlAttributes
          else:
            repElement.attrs = {"level": $skyBase.reputation.level}.toXmlAttributes
          baseTree.add(son = repElement)
        for recruit in skyBase.recruits:
          var recruitNode: XmlNode = newXmlTree(tag = "recruit", children = [],
              attributes = {"name": recruit.name, "gender": $recruit.gender,
              "price": $recruit.price,
              "payment": $recruit.payment, "homebase": $recruit.homeBase,
              "faction": recruit.faction}.toXmlAttributes)
          for skill in recruit.skills:
            var skillElement: XmlNode = newElement(tag = "skill")
            skillElement.attrs = {"index": $skill.index,
                "level": $skill.level}.toXmlAttributes
            recruitNode.add(son = skillElement)
          for attribute in recruit.attributes:
            var attrElement: XmlNode = newElement(tag = "attribute")
            attrElement.attrs = {"level": $attribute.level}.toXmlAttributes
            recruitNode.add(son = attrElement)
          for item in recruit.inventory:
            var itemElement: XmlNode = newElement(tag = "item")
            itemElement.attrs = {"index": $item.index}.toXmlAttributes
            recruitNode.add(son = itemElement)
          for index, item in recruit.equipment:
            if item > -1:
              var itemElement: XmlNode = newElement(tag = "equipment")
              itemElement.attrs = {"slot": $(index.int + 1), "index": $(item +
                  1)}.toXmlAttributes
              recruitNode.add(son = itemElement)
          baseTree.add(son = recruitNode)
        saveDate = newElement(tag = "askedforeventsdate")
        saveDate.attrs = {"year": $skyBase.askedForEvents.year,
            "month": $skyBase.askedForEvents.month,
            "day": $skyBase.askedForEvents.day}.toXmlAttributes
        baseTree.add(son = saveDate)
        saveDate = newElement(tag = "missionsdate")
        saveDate.attrs = {"year": $skyBase.missionsDate.year,
            "month": $skyBase.missionsDate.month,
            "day": $skyBase.missionsDate.day}.toXmlAttributes
        baseTree.add(son = saveDate)
        for mission in skyBase.missions:
          var
            missionElement: XmlNode = newElement(tag = "mission")
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
          baseTree.add(son = missionElement)
      if skyBase.reputation.level != 0:
        var repElement: XmlNode = newElement(tag = "reputation")
        if skyBase.reputation.experience > 0:
          repElement.attrs = {"level": $skyBase.reputation.level,
              "progress": $skyBase.reputation.experience}.toXmlAttributes
        else:
          repElement.attrs = {"level": $skyBase.reputation.level}.toXmlAttributes
        baseTree.add(son = repElement)
      for item in skyBase.cargo:
        var itemElement: XmlNode = newElement(tag = "item")
        itemElement.attrs = {"index": $item.protoIndex, "amount": $item.amount,
            "durability": $item.durability,
            "price": $item.price,
            "quality": $item.quality}.toXmlAttributes
        baseTree.add(son = itemElement)
      saveData.add(son = baseTree)

proc loadBases*(saveData: XmlNode) {.raises: [ValueError], tags: [],
    contractual.} =
  ## Load the bases from the file into the game
  ##
  ## * saveData - the XML structure from which the bases will be loaded
  require:
    saveData != nil
  body:
    var baseIndex: BasesRange = 1
    for base in saveData.findAll(tag = "base"):
      skyBases[baseIndex].name = base.attr(name = "name")
      skyBases[baseIndex].visited = DateRecord()
      skyBases[baseIndex].skyX = base.attr(name = "x").parseInt
      skyBases[baseIndex].skyY = base.attr(name = "y").parseInt
      skyBases[baseIndex].baseType = base.attr(name = "type")
      skyBases[baseIndex].population = base.attr(name = "population").parseInt
      skyBases[baseIndex].recruitDate = DateRecord()
      skyBases[baseIndex].recruits = @[]
      skyBases[baseIndex].known = false
      skyBases[baseIndex].askedForBases = false
      skyBases[baseIndex].askedForEvents = DateRecord()
      skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)
      skyBases[baseIndex].missionsDate = DateRecord()
      skyBases[baseIndex].missions = @[]
      skyBases[baseIndex].owner = base.attr(name = "owner")
      skyBases[baseIndex].size = parseEnum[BasesSize](s = base.attr(
          name = "size").toLowerAscii)
      if base.attr(name = "askedforbases") == "Y":
        skyBases[baseIndex].askedForBases = true
      if base.attr(name = "known") == "Y":
        skyBases[baseIndex].known = true
      let visitDate: XmlNode = base.child(name = "visiteddate")
      if visitDate != nil:
        skyBases[baseIndex].visited = DateRecord(year: visitDate.attr(
            name = "year").parseInt, month: visitDate.attr(
            name = "month").parseInt, day: visitDate.attr(
            name = "day").parseInt, hour: visitDate.attr(
            name = "hour").parseInt, minutes: visitDate.attr(
            name = "minutes").parseInt)
      let recruitDate: XmlNode = base.child(name = "recruitdate")
      if recruitDate != nil:
        skyBases[baseIndex].recruitDate = DateRecord(year: recruitDate.attr(
            name = "year").parseInt, month: recruitDate.attr(
            name = "month").parseInt, day: recruitDate.attr(
            name = "day").parseInt, hour: 0, minutes: 0)
      let askDate: XmlNode = base.child(name = "askedforeventsdate")
      if askDate != nil:
        skyBases[baseIndex].askedForEvents = DateRecord(year: askDate.attr(
            name = "year").parseInt, month: askDate.attr(
            name = "month").parseInt, day: askDate.attr(name = "day").parseInt,
            hour: 0, minutes: 0)
      for baseRecruit in base.findAll(tag = "recruit"):
        var recruit: RecruitData = RecruitData()
        recruit.name = baseRecruit.attr(name = "name")
        recruit.gender = baseRecruit.attr(name = "gender")[0]
        recruit.price = baseRecruit.attr(name = "price").parseInt
        recruit.payment = 20
        recruit.homeBase = 1
        for recruitSkill in baseRecruit.findAll(tag = "skill"):
          var skill: SkillInfo = SkillInfo()
          skill.index = recruitSkill.attr(name = "index").parseInt
          skill.level = recruitSkill.attr(name = "level").parseInt
          recruit.skills.add(y = skill)
        for recruitAttr in baseRecruit.findAll(tag = "attribute"):
          recruit.attributes.add(y = MobAttributeRecord(level: recruitAttr.attr(
              name = "level").parseInt, experience: 0))
        for item in baseRecruit.findAll(tag = "item"):
          recruit.inventory.add(y = RecruitItem(index: item.attr(
              name = "index").parseInt))
        for item in recruit.equipment.mitems:
          item = -1
        for equipment in baseRecruit.findAll(tag = "equipment"):
          var eqIndex: int = (equipment.attr(name = "slot").parseInt - 1)
          recruit.equipment[eqIndex.EquipmentLocations] = equipment.attr(
              name = "index").parseInt - 1
        let payment: string = baseRecruit.attr(name = "payment")
        if payment.len > 0:
          recruit.payment = payment.parseInt
        let homeBase: string = baseRecruit.attr(name = "homebase")
        if homeBase.len > 0:
          recruit.homeBase = homeBase.parseInt
        let faction: string = baseRecruit.attr(name = "faction")
        if faction.len > 0:
          recruit.faction = faction
        skyBases[baseIndex].recruits.add(y = recruit)
      let reputation: XmlNode = base.child(name = "reputation")
      if reputation != nil:
        skyBases[baseIndex].reputation.level = reputation.attr(
            name = "level").parseInt
        let progress: string = reputation.attr(name = "progress")
        if progress.len > 0:
          skyBases[baseIndex].reputation.experience = progress.parseInt
      let missionsDate: XmlNode = base.child(name = "missionsdate")
      if missionsDate != nil:
        skyBases[baseIndex].missionsDate = DateRecord(year: missionsDate.attr(
            name = "year").parseInt, month: missionsDate.attr(
                name = "month").parseInt,
            day: missionsDate.attr(name = "day").parseInt, hour: 0, minutes: 0)
      for mission in base.findAll(tag = "mission"):
        let mType: MissionsTypes = mission.attr(
            name = "type").parseInt.MissionsTypes
        var
          targetIndex: string = ""
          target: int = -1
        if mType in {deliver, destroy}:
          targetIndex = mission.attr(name = "target")
        else:
          target = mission.attr(name = "target").parseInt
        let
          time: int = mission.attr(name = "time").parseInt
          targetX: int = mission.attr(name = "targetx").parseInt
          targetY: int = mission.attr(name = "targety").parseInt
          reward: int = mission.attr(name = "reward").parseInt
        case mType
        of deliver:
          skyBases[baseIndex].missions.add(y = MissionData(mType: deliver,
              itemIndex: targetIndex.parseInt, time: time, targetX: targetX,
              targetY: targetY, reward: reward, startBase: baseIndex,
              finished: false, multiplier: 1.0))
        of destroy:
          skyBases[baseIndex].missions.add(y = MissionData(mType: destroy,
              shipIndex: targetIndex.parseInt, time: time, targetX: targetX,
              targetY: targetY, reward: reward, startBase: baseIndex,
              finished: false, multiplier: 1.0))
        of patrol:
          skyBases[baseIndex].missions.add(y = MissionData(mType: patrol,
              target: target, time: time, targetX: targetX, targetY: targetY,
              reward: reward, startBase: baseIndex, finished: false,
              multiplier: 1.0))
        of explore:
          skyBases[baseIndex].missions.add(y = MissionData(mType: explore,
              target: target, time: time, targetX: targetX, targetY: targetY,
              reward: reward, startBase: baseIndex, finished: false,
              multiplier: 1.0))
        of passenger:
          if target > 91:
            target = 91
          skyBases[baseIndex].missions.add(y = MissionData(mType: passenger,
              data: target, time: time, targetX: targetX, targetY: targetY,
              reward: reward, startBase: baseIndex, finished: false,
              multiplier: 1.0))
      for baseItem in base.findAll(tag = "item"):
        if baseItem.attr(name = "durability").len == 0:
          continue
        var item: BaseCargo = BaseCargo()
        item.protoIndex = baseItem.attr(name = "index").parseInt
        item.durability = baseItem.attr(name = "durability").parseInt
        item.amount = baseItem.attr(name = "amount").parseInt
        item.price = baseItem.attr(name = "price").parseInt
        if baseItem.attr(name = "quality").len > 0:
          item.quality = parseEnum[ObjectQuality](s = baseItem.attr(
              name = "quality"))
        else:
          item.quality = normal
        skyBases[baseIndex].cargo.add(y = item)
      skyMap[skyBases[baseIndex].skyX][skyBases[
          baseIndex].skyY].baseIndex = baseIndex
      if baseIndex < BasesRange.high:
        baseIndex.inc
