# Copyright 2022-2024 Bartek thindil Jasicki
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

## Provides code related to the in-game factions, like loading them from
## files, getting their reputation, checking do they are friendly or
## getting a random faction.

import std/[strutils, tables, xmlparser, xmltree]
import contracts
import basestypes, careers, game, items, log, types, utils

proc loadFactionData(factionNode: XmlNode; factionAction: DataAction;
    factionIndex: string; faction: var FactionData) {.raises: [
    DataLoadingError], tags: [], contractual.} =
  ## Load various data from a file for the selected faction
  ##
  ## * factionNode   - the XML node with data which will be loaded
  ## * factionAction - the action to take with the faction, like add, remove,
  ##                   etc.
  ## * factionIndex  - the index of the faction
  ## * faction       - the faction to which the data will be added
  ##
  ## Returns the modified parameter faction.
  body:
    for childNode in factionNode:
      if childNode.kind != xnElement:
        continue
      case childNode.tag
      of "relation":
        let relationIndex: string = childNode.attr(name = "faction")
        var relation: RelationsData = RelationsData()
        var attribute: string = childNode.attr(name = "reputation")
        if attribute.len() > 0:
          try:
            relation.reputation = ReputationRanges(min: attribute.parseInt(), max: 0)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                factionIndex & "', invalid value for reputation.")
        else:
          try:
            relation.reputation = ReputationRanges(min: childNode.attr(
                name = "minreputation").parseInt(), max: childNode.attr(
                name = "maxreputation").parseInt())
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                factionIndex & "', invalid value for reputation.")
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
        let foodType: string = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, food in faction.foodTypes:
            if food == foodType:
              faction.foodTypes.delete(i = index)
              break
        else:
          if findProtoItem(itemType = foodType) == 0:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                factionIndex &
                "', no items with type '" & foodType & "'.")
          faction.foodTypes.add(y = foodType)
      of "drinktype":
        let drinkType: string = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, drink in faction.drinksTypes:
            if drink == drinkType:
              faction.drinksTypes.delete(i = index)
              break
        else:
          if findProtoItem(itemType = drinkType) == 0:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                factionIndex &
                "', no items with type '" & drinkType & "'.")
          faction.drinksTypes.add(y = drinkType)
      of "career":
        let careerIndex: string = childNode.attr(name = "index")
        if childNode.attr(name = "action") == "remove":
          {.warning[ProveInit]: off.}
          {.warning[UnsafeDefault]: off.}
          faction.careers.del(key = careerIndex)
          {.warning[UnsafeDefault]: on.}
          {.warning[ProveInit]: on.}
        else:
          var career: CareerData = CareerData(shipIndex: 1)
          var attribute: string = childNode.attr(name = "shipindex")
          if attribute.len() > 0:
            try:
              career.shipIndex = attribute.parseInt()
            except ValueError:
              raise newException(exceptn = DataLoadingError,
                  message = "Can't " & $factionAction & " faction '" &
                  factionIndex & "', invalid value for ship index.")
          attribute = childNode.attr(name = "playerindex")
          if attribute.len() > 0:
            career.playerIndex = attribute
          attribute = childNode.attr(name = "name")
          if attribute.len() > 0:
            career.name = attribute
          else:
            try:
              career.name = careersList[careerIndex].name
            except KeyError:
              raise newException(exceptn = DataLoadingError,
                  message = "Can't " & $factionAction & " faction '" &
                  factionIndex &
                  "', no career with index '" & careerIndex & "'.")
          career.description = childNode.innerText
          faction.careers[careerIndex] = career
      of "basetype":
        let baseIndex: string = childNode.attr(name = "index")
        if childNode.attr(name = "action") == "remove":
          {.warning[ProveInit]: off.}
          {.warning[UnsafeDefault]: off.}
          faction.basesTypes.del(key = baseIndex)
          {.warning[UnsafeDefault]: on.}
          {.warning[ProveInit]: on.}
        elif basesTypesList.hasKey(key = baseIndex):
          try:
            faction.basesTypes[baseIndex] = childNode.attr(
                name = "chance").parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                factionIndex & "', invalid value for base spawn chance.")
      of "flag":
        let factionFlag: string = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, flag in faction.foodTypes:
            if flag == factionFlag:
              faction.flags.delete(i = index)
              break
        else:
          faction.flags.add(y = factionFlag)

proc loadFactions*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load available factions from the data file
  ##
  ## * fileName - the path to the file with factions data which will be loaded
  require:
    fileName.len > 0
  body:
    let factionsXml: XmlNode = try:
        loadXml(path = fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load factions data file. Reason: " &
            getCurrentExceptionMsg())
    for factionNode in factionsXml:
      if factionNode.kind != xnElement:
        continue
      let
        factionIndex: string = factionNode.attr(name = "index")
        factionAction: DataAction = try:
            parseEnum[DataAction](s = factionNode.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if factionAction in [update, remove]:
        if factionsList.hasKey(key = factionIndex):
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', there is no faction with that index.")
      elif factionsList.hasKey(key = factionIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't add faction '" & factionIndex & "', there is a faction with that index.")
      if factionAction == DataAction.remove:
        factionsList.del(key = factionIndex)
        logMessage(message = "Faction removed: '" & factionIndex & "'",
            debugType = everything)
        continue
      var faction: FactionData = if factionAction == DataAction.update:
          try:
            factionsList[factionIndex]
          except KeyError:
            FactionData()
        else:
          FactionData()
      var attribute: string = factionNode.attr(name = "name")
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
        try:
          faction.spawnChance = attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', invalid value for spawn chance.")
      attribute = factionNode.attr(name = "population")
      if attribute.len() > 0:
        try:
          faction.population[1] = attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', invalid value for population.")
        faction.population[2] = 0
      attribute = factionNode.attr(name = "minpopulation")
      if attribute.len() > 0:
        try:
          faction.population[1] = attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', invalid value for minpopulation.")
        try:
          faction.population[2] = factionNode.attr(
              name = "maxpopulation").parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', invalid value for maxpopulation.")
        if faction.population[2] < faction.population[1]:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', invalid range for faction's population.")
      attribute = factionNode.attr(name = "namestype")
      if attribute.len() > 0:
        faction.namesType = try:
            parseEnum[NamesTypes](s = attribute.toLowerAscii)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $factionAction & " faction '" &
                    factionIndex & "', invalid type of faction's names.")
      else:
        faction.namesType = normal
      attribute = factionNode.attr(name = "healingtools")
      if attribute.len() > 0:
        let itemIndex: Natural = findProtoItem(itemType = attribute)
        if itemIndex == 0:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex &
              "', no items with type '" & attribute & "'.")
        faction.healingTools = attribute
      attribute = factionNode.attr(name = "healingskill")
      if attribute.len() > 0:
        let skillIndex: Natural = findSkillIndex(skillName = attribute)
        if skillIndex == 0:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex &
              "', no skill named '" & attribute & "'.")
        faction.healingSkill = skillIndex
      attribute = factionNode.attr(name = "baseicon")
      if attribute.len() > 0:
        try:
          faction.baseIcon = fromHex[Natural](s = "0x" & attribute)
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex & "', invalid value for base icon.")
      attribute = factionNode.attr(name = "weaponskill")
      if attribute.len() > 0:
        let skillIndex: Natural = findSkillIndex(skillName = attribute)
        if skillIndex == 0:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $factionAction & " faction '" &
              factionIndex &
              "', no skill named '" & attribute & "'.")
        faction.weaponSkill = skillIndex
      loadFactionData(factionNode = factionNode, factionAction = factionAction,
          factionIndex = factionIndex, faction = faction)
      if factionAction == DataAction.add:
        if faction.basesTypes.len() == 0:
          for key in basesTypesList.keys:
            faction.basesTypes[key] = 20
        logMessage(message = "Faction added: '" & factionIndex & "'",
            debugType = everything)
      else:
        logMessage(message = "Faction updated: '" & factionIndex & "'",
            debugType = everything)
      factionsList[factionIndex] = faction

proc getReputation*(sourceFaction, targetFaction: string): int {.sideEffect,
    raises: [KeyError], tags: [], contractual.} =
  ## Get the reputation level between the two factions
  ##
  ## * sourceFaction - the faction which repuration level will be get
  ## * targetFaction - the faction to which reputation level will be get
  ##
  ## Returns the level of the reputation between factions. If only one value is set
  ## return value, if both, return random value between them.
  require:
    factionsList.hasKey(key = sourceFaction)
    factionsList.hasKey(key = targetFaction)
  body:
    if factionsList[sourceFaction].relations[targetFaction].reputation.max == 0:
      return factionsList[sourceFaction].relations[targetFaction].reputation.min
    return getRandom(min = factionsList[sourceFaction].relations[
        targetFaction].reputation.min, max = factionsList[
        sourceFaction].relations[targetFaction].reputation.max)

proc isFriendly*(sourceFaction, targetFaction: string): bool {.sideEffect,
    raises: [KeyError], tags: [], contractual.} =
  ## Check if the selected factions are friendly towards self
  ##
  ## * sourceFaction - the faction which will be checked for friendliness
  ## * targetFaction - the faction towards which check will be make
  ##
  ## Returns true if factions are friendly towards self, otherwise false.
  require:
    factionsList.hasKey(key = sourceFaction)
    factionsList.hasKey(key = targetFaction)
  body:
    return factionsList[sourceFaction].relations[targetFaction].friendly

proc getRandomFaction*(): string {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Get the index of the random faction
  ##
  ## Returns the index of the random faction
  let factionIndex: Positive = getRandom(min = 1, max = factionsList.len)
  var currentIndex: Positive = 1
  for key in factionsList.keys:
    if currentIndex == factionIndex:
      return key
    currentIndex.inc

# Temporary code for interfacing with Ada

type
  AdaFactionData* = object
    ## Temporary C binding
    name: cstring
    memberName: cstring
    pluralMemberName: cstring
    spawnChance: cint
    population: array[1..2, cint]
    namesType: cint
    description: cstring
    healingTools: cstring
    healingSkill: cint
    baseIcon: cint
    weaponSkill: cint

  AdaCareerData* = object
    ## Temporary C binding
    shipIndex: cint
    playerIndex: cstring
    description: cstring
    name: cstring

proc getAdaFactionIndex(index: cint): cstring {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  var factionNumber: Positive = 1
  for factionIndex in factionsList.keys:
    if index == factionNumber:
      return factionIndex.cstring
    factionNumber.inc

proc getAdaFaction(index: cstring; numericIndex: cint;
    adaFaction: var AdaFactionData) {.sideEffect, raises: [], tags: [], exportc,
        contractual.} =
  ## Temporary C binding
  adaFaction = AdaFactionData(name: "".cstring, memberName: "".cstring,
      pluralMemberName: "".cstring, spawnChance: 0, population: [1: 0.cint,
          2: 0.cint], namesType: 0, description: "".cstring,
          healingTools: "".cstring, healingSkill: 0, baseIcon: 0,
          weaponSkill: 0)
  try:
    var faction: FactionData = FactionData()
    if numericIndex > 0:
      faction = factionsList[$getAdaFactionIndex(index = numericIndex)]
    else:
      faction = factionsList[$index]
    adaFaction.name = faction.name.cstring
    adaFaction.memberName = faction.memberName.cstring
    adaFaction.pluralMemberName = faction.pluralMemberName.cstring
    adaFaction.spawnChance = faction.spawnChance.cint
    adaFaction.population = [1: faction.population[1].cint,
        2: faction.population[2].cint]
    adaFaction.namesType = faction.namesType.ord().cint
    adaFaction.description = faction.description.cstring
    adaFaction.healingTools = faction.healingTools.cstring
    adaFaction.healingSkill = faction.healingSkill.cint
    adaFaction.baseIcon = faction.baseIcon.cint
    adaFaction.weaponSkill = faction.weaponSkill.cint
  except KeyError:
    discard

proc getAdaFactionData(factionIndex: cstring; index: cint;
    adaDataType: cstring): cstring {.sideEffect, raises: [], tags: [], exportc,
        contractual.} =
  ## Temporary C binding
  try:
    let dataList: seq[string] = case $adaDataType
      of "foodType":
        factionsList[$factionIndex].foodTypes
      of "drinkType":
        factionsList[$factionIndex].drinksTypes
      of "flag":
        factionsList[$factionIndex].flags
      else:
        return ""
    if index >= dataList.len():
      return ""
    return dataList[index].cstring
  except KeyError:
    return ""

proc getAdaFactionRelation(factionIndex: cstring; index: cint;
    relation: var array[3, cint]): cstring {.sideEffect, raises: [], tags: [],
        exportc, contractual.} =
  ## Temporary C binding
  relation = [0.cint, 0.cint, 0.cint]
  try:
    if index > factionsList[$factionIndex].relations.len():
      return ""
    var currIndex: Natural = 0
    for relIndex, factionRelation in factionsList[
        $factionIndex].relations:
      currIndex.inc()
      if currIndex < index:
        continue
      relation = [factionRelation.reputation.min.cint,
          factionRelation.reputation.max.cint, factionRelation.friendly.ord().cint]
      return relIndex.cstring
  except KeyError:
    return ""

proc getAdaFactionCareer(factionIndex: cstring; index: cint;
    career: var AdaCareerData): cstring {.sideEffect, raises: [], tags: [],
        exportc, contractual.} =
  ## Temporary C binding
  career = AdaCareerData(shipIndex: 1, playerIndex: "".cstring,
      description: "".cstring, name: "".cstring)
  try:
    if index > factionsList[$factionIndex].careers.len():
      return ""
    var currIndex: Natural = 0
    for carIndex, factionCareer in factionsList[$factionIndex].careers:
      currIndex.inc()
      if currIndex < index:
        continue
      career.shipIndex = factionCareer.shipIndex.cint
      career.playerIndex = factionCareer.playerIndex.cstring
      career.description = factionCareer.description.cstring
      career.name = factionCareer.name.cstring
      return carIndex.cstring
  except KeyError:
    return ""

proc getAdaFactionBase(factionIndex: cstring; index: cint;
    baseIndex: var cint): cstring {.sideEffect, raises: [], tags: [], exportc,
        contractual.} =
  ## Temporary C binding
  baseIndex = 0
  try:
    if index > factionsList[$factionIndex].basesTypes.len():
      return ""
    var currIndex: Natural = 0
    for bIndex, factionBase in factionsList[$factionIndex].basesTypes:
      currIndex.inc()
      if currIndex < index:
        continue
      baseIndex = factionBase.cint
      return bIndex.cstring
  except KeyError:
    return ""

proc getAdaReputation(sourceFaction, targetFaction: cstring): cint {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    return getReputation(sourceFaction = $sourceFaction,
        targetFaction = $targetFaction).cint
  except KeyError:
    return 0

proc isAdaFriendly(sourceFaction, targetFaction: cstring): cint {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    return isFriendly(sourceFaction = $sourceFaction,
        targetFaction = $targetFaction).ord.cint
  except KeyError:
    return 0

proc getAdaRandomFaction(): cstring {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  return getRandomFaction().cstring

proc getAdaFactionsAmount(): cint {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  return factionsList.len.cint
