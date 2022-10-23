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
import game, items, log

type
  NamesTypes = enum
    normal, robotic

  ReputationRanges = object
    min: ReputationRange
    max: ReputationRange

  RelationsData = object
    reputation: ReputationRanges
    friendly: bool

  CareerData = object
    shipIndex: Positive
    playerIndex: string
    description: string
    name: string

  FactionData = object
    name: string
    memberName: string
    pluralMemberName: string
    spawnChance: Natural
    population: AttributesArray
    namesType: NamesTypes
    relations: Table[string, RelationsData]
    description: string
    foodTypes: seq[string]
    drinksTypes: seq[string]
    healingTools: string
    healingSkill: Natural
    flags: seq[string]
    careers: Table[string, CareerData]
    baseIcon: Natural
    basesTypes: Table[string, Positive]
    weaponSkill: Natural

var factionsList*: Table[string, FactionData] = initTable[string, FactionData]()

proc loadFactions*(fileName: string) =
  let factionsXml = loadXml(path = fileName)
  for factionNode in factionsXml:
    if factionNode.kind != xnElement:
      continue
    let
      factionIndex = factionNode.attr(name = "index")
      factionAction: DataAction = try:
          parseEnum[DataAction](factionNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if factionAction in [update, remove]:
      if factionsList.hasKey(key = factionIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', there is no faction with that index,")
    elif factionsList.hasKey(key = factionIndex):
      raise newException(exceptn = DataLoadingError,
          message = "Can't add faction '" & factionIndex & "', there is a faction with that index.")
    if factionAction == DataAction.remove:
      factionsList.del(key = factionIndex)
      logMessage(message = "Faction removed: '" & factionIndex & "'",
          debugType = everything)
      continue
    var faction: FactionData = if factionAction == DataAction.update:
        factionsList[factionIndex]
      else:
        FactionData()
    var attribute = factionNode.attr(name = "name")
    if attribute.len() > 0:
      faction.name = attribute
    attribute = factionNode.attr(name = "membername")
    if attribute.len() > 0:
      faction.memberName = attribute
    attribute = factionNode.attr(name = "pluralmembername")
    if attribute.len() > 0:
      faction.pluralMemberName = attribute
    attribute = factionNode.attr(name = "spawn")
    if attribute.len() > 0:
      faction.spawnChance = attribute.parseInt()
    attribute = factionNode.attr(name = "population")
    if attribute.len() > 0:
      faction.population[1] = attribute.parseInt()
      faction.population[2] = 0
    attribute = factionNode.attr(name = "minpopulation")
    if attribute.len() > 0:
      faction.population[1] = attribute.parseInt()
      faction.population[2] = factionNode.attr(name = "maxpopulation").parseInt()
      if faction.population[2] < faction.population[1]:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', invalid range for faction's population.")
    attribute = factionNode.attr(name = "namestype")
    if attribute.len() > 0:
      faction.namesType = try:
          parseEnum[NamesTypes](attribute.toLowerAscii)
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
                  factionIndex & "', invalid type of faction's names.")
    attribute = factionNode.attr(name = "healingtools")
    if attribute.len() > 0:
      let itemIndex = findProtoItem(itemType = attribute)
      if itemIndex == 0:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex &
            "', no items with type '" & attribute & "'.")
      faction.healingTools = attribute
    attribute = factionNode.attr(name = "healingskill")
    if attribute.len() > 0:
      let skillIndex = findSkillIndex(skillName = attribute)
      if skillIndex == 0:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex &
            "', no skill named '" & attribute & "'.")
      faction.healingSkill = skillIndex
    attribute = factionNode.attr(name = "baseicon")
    if attribute.len() > 0:
      faction.baseIcon = ("0x" & attribute).parseInt()
    attribute = factionNode.attr(name = "weaponskill")
    if attribute.len() > 0:
      let skillIndex = findSkillIndex(skillName = attribute)
      if skillIndex == 0:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex &
            "', no skill named '" & attribute & "'.")
      faction.weaponSkill = skillIndex
    for childNode in factionNode:
      if childNode.kind != xnElement:
        continue
      case childNode.tag
      of "relation":
        let relationIndex = childNode.attr(name = "faction")
        var relation: RelationsData
        attribute = childNode.attr(name = "reputation")
        if attribute.len() > 0:
          relation.reputation = ReputationRanges(min: attribute.parseInt(), max: 0)
        else:
          relation.reputation = ReputationRanges(min: childNode.attr(
              name = "minreputation").parseInt(), max: childNode.attr(
              name = "maxreputation").parseInt())
          if relation.reputation.min > relation.reputation.max:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                factionIndex &
                "', invalid range for faction's reputation with '" &
                relationIndex & "'.")
        if childNode.attr(name = "friendly") == "Y":
          relation.friendly = true
        else:
          relation.friendly = false
        faction.relations[relationIndex] = relation
      of "description":
        faction.description = childNode.innerText()
      of "foodtype":
        let foodType = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, food in faction.foodTypes.pairs:
            if food == foodType:
              faction.foodTypes.delete(i = index)
              break
        else:
          if findProtoItem(itemType = foodType) == 0:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" & factionIndex &
                "', no items with type '" & foodType & "'.")
          faction.foodTypes.add(y = foodType)
      of "drinktype":
        let drinkType = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, drink in faction.drinksTypes.pairs:
            if drink == drinkType:
              faction.drinksTypes.delete(i = index)
              break
        else:
          if findProtoItem(itemType = drinkType) == 0:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" & factionIndex &
                "', no items with type '" & drinkType & "'.")
          faction.drinksTypes.add(y = drinkType)
      of "career":
        let careerIndex = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          {.warning[ProveInit]: off.}
          {.warning[UnsafeDefault]: off.}
          faction.careers.del(key = careerIndex)
          {.warning[UnsafeDefault]: on.}
          {.warning[ProveInit]: on.}
        else:
          var career = CareerData(shipIndex: 1)
          attribute = childNode.attr(name = "shipindex")
          if attribute.len() > 0:
            career.shipIndex = attribute.parseInt()
          attribute = childNode.attr(name = "playerindex")
          if attribute.len() > 0:
            career.playerIndex = attribute
          attribute = childNode.attr(name = "name")
          if attribute.len() > 0:
            career.name = attribute
          career.description = childNode.innerText
          faction.careers[careerIndex] = career

proc loadAdaFactions*(fileName: cstring) {.exportc.} =
  loadFactions(fileName = $fileName)
