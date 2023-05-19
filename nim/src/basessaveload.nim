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
