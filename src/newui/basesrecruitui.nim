# Copyright 2025 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to recruit new crew members in bases, like show the
## UI, start negotiating, show information about a recruit, etc.

import std/[algorithm, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, maps, types]
import coreui, header, messagesui, table

type
  RecruitsSortOrders = enum
    none, nameAsc, nameDesc, genderAsc, genderDesc, factionAsc, factionDesc,
      priceAsc, priceDesc, attributeAsc, attributeDesc, skillAsc, skillDesc
  LocalRecruitData = object
    name: string
    gender: char
    faction: string
    price: Positive = 1
    attribute: string
    skill: string
    id: Natural = 0

const defaultRecruitsSortOrder: RecruitsSortOrders = none

var recruitsSortOrder: RecruitsSortOrders = defaultRecruitsSortOrder

proc sortRecruits(x, y: LocalRecruitData): int =
  ## Check how to sort the selected recruits on the list
  ##
  ## * x - the first recruit to sort
  ## * y - the second recruit to sort
  ##
  ## Returns 1 if the x recruit should go first, otherwise -1
  case recruitsSortOrder
  of nameAsc:
    if x.name < y.name:
      return 1
    else:
      return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    else:
      return -1
  of genderAsc:
    if x.gender < y.gender:
      return 1
    else:
      return -1
  of genderDesc:
    if x.gender > y.gender:
      return 1
    else:
      return -1
  of factionAsc:
    if x.faction < y.faction:
      return 1
    else:
      return -1
  of factionDesc:
    if x.faction > y.faction:
      return 1
    else:
      return -1
  of priceAsc:
    if x.price < y.price:
      return 1
    else:
      return -1
  of priceDesc:
    if x.price > y.price:
      return 1
    else:
      return -1
  of attributeAsc:
    if x.attribute < y.attribute:
      return 1
    else:
      return -1
  of attributeDesc:
    if x.attribute > y.attribute:
      return 1
    else:
      return -1
  of skillAsc:
    if x.skill < y.skill:
      return 1
    else:
      return -1
  of skillDesc:
    if x.skill > y.skill:
      return 1
    else:
      return -1
  of none:
    return -1

proc getHighestAttribute(baseIndex: BasesRange;
    memberIndex: Natural): string {.raises: [], tags: [], contractual.} =
  ## Get the highest attribute's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's attributes
  ##                 will be check
  ## * memberIndex - The index of the recruit which attributes will be check
  ##
  ## Returns the name of the attribute with the highest level of the selected
  ## recruit
  var
    highestLevel: Positive = 1
    highestIndex: Natural = 0
  for index, attrib in skyBases[baseIndex].recruits[memberIndex].attributes:
    if attrib.level > highestLevel:
      highestLevel = attrib.level
      highestIndex = index
  return attributesList[highestIndex].name

proc getHighestSkill(baseIndex: BasesRange;
    memberIndex: Natural): string {.raises: [], tags: [], contractual.} =
  ## Get the highest skill's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's skill will
  ##                 be check
  ## * memberIndex - The index of the recruit which skills will be check
  ##
  ## Returns the name of the skill with the highest level of the selected
  ## recruit
  var
    highestLevel: Positive = 1
    highestIndex: Natural = 0
  for skill in skyBases[baseIndex].recruits[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except:
    return ""

var recruitsIndexes: seq[Natural] = @[]

proc sortRecruits(sortAsc, sortDesc: RecruitsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort recruits on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if recruitsSortOrder == sortAsc:
    recruitsSortOrder = sortDesc
  else:
    recruitsSortOrder = sortAsc
  var localRecruits: seq[LocalRecruitData]
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  for index, recruit in skyBases[baseIndex].recruits:
    localRecruits.add(y = LocalRecruitData(name: recruit.name,
        gender: recruit.gender, faction: recruit.faction, price: recruit.price,
        attribute: getHighestAttribute(baseIndex = baseIndex,
        memberIndex = index), skill: getHighestSkill(baseIndex = baseIndex,
        memberIndex = index), id: index))
  localRecruits.sort(cmp = sortRecruits)
  recruitsIndexes = @[]
  for recruit in localRecruits:
    recruitsIndexes.add(y = recruit.id)

const
  headers: array[6, HeaderData[RecruitsSortOrders]] = [
    HeaderData[RecruitsSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[RecruitsSortOrders](label: "Gender", sortAsc: genderAsc,
        sortDesc: genderDesc),
    HeaderData[RecruitsSortOrders](label: "Faction", sortAsc: factionAsc,
        sortDesc: factionDesc),
    HeaderData[RecruitsSortOrders](label: "Base cost", sortAsc: priceAsc,
        sortDesc: priceDesc),
    HeaderData[RecruitsSortOrders](label: "Highest stat", sortAsc: attributeAsc,
        sortDesc: attributeDesc),
    HeaderData[RecruitsSortOrders](label: "Highest skill", sortAsc: skillAsc,
        sortDesc: skillDesc)]
  ratio: array[6, cfloat] = [300.cfloat, 200, 200, 200, 200, 200]

proc setRecruits*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for recruits UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  if recruitsIndexes.len != skyBases[baseIndex].recruits.len:
    recruitsIndexes = @[]
    for index, _ in skyBases[baseIndex].recruits:
      recruitsIndexes.add(y = index)

proc showRecruits*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the recruits UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "RecruitsGroup", flags = {windowNoFlags}):
    addHeader(headers = headers, ratio = ratio, tooltip = "recruits",
      code = sortRecruits, dialog = dialog)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
  showGameMenu(dialog = dialog)
