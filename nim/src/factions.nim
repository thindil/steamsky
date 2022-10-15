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
import game

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
    healingTools: seq[string]
    healingSkill: Natural
    flags: seq[string]
    careers: Table[string, CareerData]
    baseIcon: Natural
    basesTypes: Table[string, Positive]
    weaponSkill: Natural

  DataLoadingError = object of CatchableError

var factionsList*: Table[string, FactionData] = initTable[string, FactionData]()

proc loadFactions*(fileName: string) =
  let factionsXml = loadXml(path = fileName)
  for factionNode in factionsXml:
    var faction: FactionData
    let
      factionIndex = try:
          factionNode.attr(name = "index")
        except:
          ""
      factionAction: DataAction = try:
          parseEnum[DataAction](factionNode.attr(name = "action").toLowerAscii)
        except:
          DataAction.add
    if factionAction in [update, remove]:
      if factionsList.hasKey(key = factionIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $factionAction & " faction '" & factionIndex & "', there is no faction with that index,")
    elif factionsList.hasKey(key = factionIndex):
        raise newException(exceptn = DataLoadingError,
            message = "Can't add faction '" & factionIndex & "', there is a faction with that index.")

proc loadAdaFactions*(fileName: cstring) {.exportc.} =
  loadFactions(fileName = $fileName)
