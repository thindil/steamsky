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
import basestypes, careers, game, items, log

type
  NamesTypes* = enum
    ## FUNCTION
    ##
    ## The type of the faction names, normal, based on syllables or robotic,
    ## based on random letters and numbers
    normal, robotic

  ReputationRanges = object
    ## FUNCTION
    ##
    ## Used to store reputation ranges for relation with other factions
    min: ReputationRange ## Minimal reputation with the selected faction
    max: ReputationRange ## Maximal reputation with the selected faction

  RelationsData = object
    ## FUNCTION
    ##
    ## Used to store data about relation with other faction
    reputation: ReputationRanges ## Values of min and max reputation with the faction
    friendly: bool ## If true, the selected faction is friendly towards the faction

  CareerData = object
    ## FUNCTION
    ##
    ## Used to store data about careers available for the faction
    shipIndex: Positive ## The index of the starting ship prototype for the career
    playerIndex: string ## The index of the starting mob prototype as the player character
    description: string ## The description of the career
    name: string ## The name of the career

  FactionData = object
    ## FUNCTION
    ##
    ## Used to store data about the selected faction
    name*: string ## The name of the faction
    memberName: string ## The name of members of the faction
    pluralMemberName: string ## The name for plural amount of members of the faction
    spawnChance: Natural ## The chance of the spawn for a base of the selected faction
    population: AttributesArray ## The min and max population for new bases of the faction
    namesType*: NamesTypes ## The type of names for the mobs, bases and ships of the faction
    relations: Table[string, RelationsData] ## The faction's relations with other factions
    description: string ## The description of the faction
    foodTypes: seq[string] ## The types of items used as food for the faction's members
    drinksTypes: seq[string] ## The types of items used as drinks for the faction's members
    healingTools: string ## The type of items used as healing tools for the faction's members
    healingSkill: Natural ## The skill used as healing skill for the faction's members
    flags: seq[string] ## Various flags set for the faction
    careers: Table[string, CareerData] ## The list of available careers for the faction
    baseIcon: Natural ## The icon used as icon for the faction's bases on the map
    basesTypes: Table[string, Positive] ## The list of available bases types for the faction
    weaponSkill: Natural ## The skill used as prefered weapon skill for the faction

var factionsList*: Table[string, FactionData] = initTable[string, FactionData]()
  ## FUNCTION
  ##
  ## The list of all available factions in the game

proc loadFactions*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## FUNCTION
  ##
  ## Load available factions from the data file
  ##
  ## PARAMETERS
  ##
  ## * fileName - the path to the file with factions data which will be loaded
  let factionsXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load factions data file. Reason: " &
          getCurrentExceptionMsg())
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
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', there is no faction with that index.")
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
      try:
        faction.spawnChance = attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', invalid value for spawn chance.")
    attribute = factionNode.attr(name = "population")
    if attribute.len() > 0:
      try:
        faction.population[1] = attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', invalid value for population.")
      faction.population[2] = 0
    attribute = factionNode.attr(name = "minpopulation")
    if attribute.len() > 0:
      try:
        faction.population[1] = attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', invalid value for minpopulation.")
      try:
        faction.population[2] = factionNode.attr(
            name = "maxpopulation").parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', invalid value for maxpopulation.")
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
    else:
      faction.namesType = normal
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
      try:
        faction.baseIcon = fromHex[Natural]("0x" & attribute)
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', invalid value for base icon.")
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
        let foodType = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, food in faction.foodTypes.pairs:
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
        let drinkType = childNode.attr(name = "name")
        if childNode.attr(name = "action") == "remove":
          for index, drink in faction.drinksTypes.pairs:
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
        let careerIndex = childNode.attr(name = "index")
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
        let baseIndex = childNode.attr(name = "index")
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

# Temporary code for interfacing with Ada

type
  AdaFactionData* = object
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
    shipIndex: cint
    playerIndex: cstring
    description: cstring
    name: cstring

proc loadAdaFactions*(fileName: cstring) {.sideEffect, raises: [
    DataLoadingError], tags: [WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  loadFactions(fileName = $fileName)

proc getAdaFaction(index: cint; adaFaction: var AdaFactionData): cstring {.sideEffect,
    raises: [], tags: [], exportc.} =
  adaFaction = AdaFactionData(name: "".cstring, memberName: "".cstring,
      pluralMemberName: "".cstring, spawnChance: 0, population: [1: 0.cint,
          2: 0.cint], namesType: 0, description: "".cstring,
          healingTools: "".cstring, healingSkill: 0, baseIcon: 0,
          weaponSkill: 0)
  if index > factionsList.len():
    return ""
  var
    faction: FactionData
    factionIndex: Positive = 1
  for key in factionsList.keys:
    if factionIndex == index:
      try:
        faction = factionsList[key]
        result = key.cstring
      except KeyError:
        return ""
      break
    factionIndex.inc()
  adaFaction.name = faction.name.cstring
  adaFaction.memberName = faction.memberName.cstring
  adaFaction.pluralMemberName = faction.pluralMemberName.cstring
  adaFaction.spawnChance = faction.spawnChance.cint
  adaFaction.population = [1: faction.population[1].cint, 2: faction.population[2].cint]
  adaFaction.namesType = faction.namesType.ord().cint
  adaFaction.description = faction.description.cstring
  adaFaction.healingTools = faction.healingTools.cstring
  adaFaction.healingSkill = faction.healingSkill.cint
  adaFaction.baseIcon = faction.baseIcon.cint
  adaFaction.weaponSkill = faction.weaponSkill.cint

proc getAdaFactionData(factionIndex: cstring; index: cint;
    adaDataType: cstring): cstring {.sideEffect, raises: [], tags: [], exportc.} =
  try:
    let dataList = case $adaDataType
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
    relation: var array[3, cint]): cstring {.sideEffect, raises: [], tags: [], exportc.} =
  relation = [0.cint, 0.cint, 0.cint]
  try:
    if index > factionsList[$factionIndex].relations.len():
      return ""
    var currIndex = 0
    for relIndex, factionRelation in factionsList[
        $factionIndex].relations.pairs:
      currIndex.inc()
      if currIndex < index:
        continue
      relation = [factionRelation.reputation.min.cint,
          factionRelation.reputation.max.cint, factionRelation.friendly.ord().cint]
      return relIndex.cstring
  except KeyError:
    return ""

proc getAdaFactionCareer(factionIndex: cstring; index: cint;
    career: var AdaCareerData): cstring {.sideEffect, raises: [], tags: [], exportc.} =
  career = AdaCareerData(shipIndex: 1, playerIndex: "".cstring,
      description: "".cstring, name: "".cstring)
  try:
    if index > factionsList[$factionIndex].careers.len():
      return ""
    var currIndex = 0
    for carIndex, factionCareer in factionsList[$factionIndex].careers.pairs:
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
    baseIndex: var cint): cstring {.sideEffect, raises: [], tags: [], exportc.} =
  baseIndex = 0
  try:
    if index > factionsList[$factionIndex].basesTypes.len():
      return ""
    var currIndex = 0
    for bIndex, factionBase in factionsList[$factionIndex].basesTypes.pairs:
      currIndex.inc()
      if currIndex < index:
        continue
      baseIndex = factionBase.cint
      return bIndex.cstring
  except KeyError:
    return ""
