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
import ../[config, crew, game, maps, types]
import coreui, dialogs, errordialog, header, messagesui, table, themes

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

var
  recruitsIndexes: seq[Natural] = @[]
  currentPage: Positive = 1
  baseIndex: ExtendedBasesRange = 0

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
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if recruitsIndexes.len != skyBases[baseIndex].recruits.len:
    recruitsIndexes = @[]
    for index, _ in skyBases[baseIndex].recruits:
      recruitsIndexes.add(y = index)
  currentPage = 1

var
  currentTab: cint = 0
  recruitIndex: int = -1

proc showRecruitInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected recruit information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 400

  let
    recruit: RecruitData = skyBases[baseIndex].recruits[recruitIndex]
    windowName: string = recruit.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle}):
    changeStyle(field = spacing, x = 0, y = 0):
      changeStyle(field = buttonRounding, value = 0):
        setLayoutRowDynamic(height = 30, cols = 4)
        const tabs: array[4, string] = ["General", "Attributes", "Skills", "Inventory"]
        for index, tab in tabs:
          try:
            if currentTab == index:
              saveButtonStyle()
              setButtonStyle2(source = active, destination = normal)
              labelButton(title = tab):
                discard
              restoreButtonStyle()
            else:
              labelButton(title = tab):
                currentTab = index.cint
          except:
            dialog = setError(message = "Can't set the tabs buttons.")
    setLayoutRowDynamic(height = height - 125, cols = 1)
    group(title = "InfoGroup", flags = {windowNoFlags}):
      case currentTab
      # General info about the selected recruit
      of 0:
        setLayoutRowDynamic(height = 35, cols = 2)
        label(str = "Gender:")
        colorLabel(str = if recruit.gender == 'M': "Male" else: "Female",
            color = theme.colors[goldenColor])
        let faction: FactionData = try:
            factionsList[recruit.faction]
          except:
            dialog = setError(message = "Can't get the recruit's faction.")
            return
        label(str = "Faction:")
        colorLabel(str = faction.name, color = theme.colors[goldenColor])
        label(str = "Home base:")
        colorLabel(str = skyBases[recruit.homeBase].name,
            color = theme.colors[goldenColor])
      # Statistics of the selected recruit
      of 1:
        for index, attrib in recruit.attributes:
          setLayoutRowStatic(height = 35, cols = 3, ratio = [120.cfloat, 120, 35])
          label(str = attributesList[index].name & ":")
          colorLabel(str = getAttributeLevelName(
              attributeLevel = attrib.level), color = theme.colors[goldenColor])
          imageButton(image = images[helpIcon]):
            discard
      else:
        discard
    # Buttons
    setLayoutRowDynamic(height = 30, cols = 2)
    imageLabelButton(image = images[negotiateIcon], text = "Negotiate",
        alignment = right):
      dialog = negotiateDialog
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

proc setRecruitInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [],
    contractual.} =
  ## Set the data needed for show information about the selected recruit
  ##
  ## * data
  ## * dialog
  recruitIndex = data
  dialog = recruitDialog

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
  # Show the list of recruits to hire
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "RecruitsGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    addHeader(headers = headers, ratio = ratio, tooltip = "recruits",
      code = sortRecruits, dialog = dialog)
    var currentRow = 1
    let startRow = ((currentPage - 1) * gameSettings.listsLimit) + 1
    saveButtonStyle()
    setButtonStyle(field = borderColor, a = 0)
    try:
      setButtonStyle(field = normal, color = theme.colors[tableRowColor])
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    except:
      dialog = setError(message = "Can't set table color")
      return
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    var row: Positive = 1
    for index in recruitsIndexes:
      if currentRow < startRow:
        currentRow.inc
        continue
      addButton(label = skyBases[baseIndex].recruits[index].name,
          tooltip = "Show the recruit's details.", data = index,
          code = setRecruitInfo, dialog = dialog)
      addButton(label = (if skyBases[baseIndex].recruits[index].gender ==
          'F': "Female" else: "Male"), tooltip = "Show recruit's details",
          data = index, code = setRecruitInfo, dialog = dialog)
      try:
        addButton(label = factionsList[skyBases[baseIndex].recruits[
            index].faction].name, tooltip = "Show recruit's details",
            data = index, code = setRecruitInfo, dialog = dialog)
      except:
        dialog = setError(message = "Can't get the recruit faction.")
      addButton(label = $skyBases[baseIndex].recruits[index].price,
          tooltip = "Show recruit's details", data = index,
          code = setRecruitInfo, dialog = dialog)
      addButton(label = getHighestAttribute(baseIndex = baseIndex,
          memberIndex = index), tooltip = "Show recruit's details",
          data = index, code = setRecruitInfo, dialog = dialog)
      addButton(label = getHighestSkill(baseIndex = baseIndex,
          memberIndex = index), tooltip = "Show recruit's details",
          data = index, code = setRecruitInfo, dialog = dialog)
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
    # Show the list of items in the player's ship's cargo
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
