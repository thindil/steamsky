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

import std/[algorithm, tables, strutils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basestrade, config, crew, game, items, maps, shipscrew, types]
import coreui, dialogs, errordialog, header, messagesui, setui, table, themes

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

proc sortRecruits(x, y: LocalRecruitData): int {.raises: [], tags: [],
    contractual.} =
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
  var highestLevel, highestIndex: Positive = 1
  for skill in skyBases[baseIndex].recruits[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except:
    return ""

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
  contractLength: array[5, string] = ["Pernament", "100 days", "30 days",
      "20 days", "10 days"]

var
  currentTab: cint = 0
  recruitIndex: int = -1
  currentDaily, maxDaily: Positive = 1
  currentProfit, currentContract, cost, moneyAmount: Natural = 0
  moneyText: seq[string] = @[]
  moneyWidth: seq[cfloat] = @[]
  hireText: array[3, string] = ["Hire for ", "0", " " & moneyName]
  hireWidth: array[3, cfloat] = [0, 0, 0]

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
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    changeStyle(field = spacing, x = 0, y = 0):
      changeStyle(field = buttonRounding, value = 0):
        setLayoutRowDynamic(height = 30, cols = 4)
        const tabs: array[4, string] = ["General", "Attributes", "Skills", "Inventory"]
        for index, tab in tabs:
          try:
            if currentTab == index:
              changeStyle(src = active, dest = normal):
                labelButton(title = tab):
                  discard
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
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Show detailed information about the selected attribute.")
          imageButton(image = images[helpIcon]):
            let attribute: AttributeRecord = attributesList[index]
            dialog = setInfo(text = attribute.description,
                title = attribute.name)
          setLayoutRowDynamic(height = 20, cols = 1)
          var value: int = attrib.level
          progressBar(value = value, maxValue = 50, modifyable = false)
      # Skills of the selected recruit
      of 2:
        for skill in recruit.skills:
          try:
            setLayoutRowStatic(height = 35, cols = 3, ratio = [120.cfloat, 120, 35])
            label(str = skillsList[skill.index].name & ":")
            colorLabel(str = getSkillLevelName(skillLevel = skill.level),
                color = theme.colors[goldenColor])
            if gameSettings.showTooltips:
              addTooltip(bounds = getWidgetBounds(),
                  text = "Show detailed information about the selected skill.")
            imageButton(image = images[helpIcon]):
              let skill: SkillRecord = skillsList[skill.index]
              dialog = setInfo(text = skill.description, title = skill.name)
            setLayoutRowDynamic(height = 20, cols = 1)
            var value: int = skill.level
            progressBar(value = value, maxValue = 100, modifyable = false)
          except:
            dialog = setError(message = "Can't show the recruit's skills.")
            break
      # Equipment of the selected recruit
      of 3:
        for index, item in recruit.equipment:
          if item > -1:
            setLayoutRowDynamic(height = 35, cols = 2)
            label(str = ($index).capitalizeAscii & ":")
            try:
              let rItem: RecruitItem = recruit.inventory[item]
              colorLabel(str = itemsList[rItem.index].name & (
                  if rItem.quality == normal: "" else: " (" & $rItem.quality &
                  ")"), color = theme.colors[goldenColor])
            except:
              dialog = setError(message = "Can't show the recruit's equipment")
              break
      else:
        discard
    # Buttons
    setLayoutRowDynamic(height = 30, cols = 2)
    imageLabelButton(image = images[negotiateIcon], text = "Negotiate",
        alignment = right):
      dialog = negotiateDialog
      currentDaily = recruit.payment
      maxDaily = recruit.payment * 2
      currentProfit = 0
      currentContract = 0
      moneyAmount = moneyAmount(inventory = playerShip.cargo)
      moneyText = @[]
      moneyWidth = @[]
      if moneyAmount == 0:
        moneyText.add(y = "You don't have enough money to recruit anyone")
      else:
        moneyText.add(y = "You have ")
        moneyText.add(y = $moneyAmount)
        moneyText.add(y = " " & moneyName)
      for text in moneyText:
        try:
          moneyWidth.add(y = text.getTextWidth)
        except:
          dialog = setError(message = "Can't get the width of the money text.")
          return
      cost = recruit.price
      try:
        countPrice(price = cost, traderIndex = findMember(order = talk))
      except:
        dialog = setError(message = "Can't count hire cost.")
        return
      hireText[1] = $cost
      for index, text in hireText:
        try:
          hireWidth[index] = text.getTextWidth
        except:
          dialog = setError(message = "Can't get the width of the hire text.")
          return
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

proc showNegotiate*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the negotiate dialog
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
    windowName: string = "Negotiate with " & recruit.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Daily payment:")
    let newValue: int = property2(name = "#", min = 1, val = currentDaily,
        max = maxDaily, step = 1, incPerPixel = 1)
    if newValue != currentDaily:
      currentDaily = newValue
    setLayoutRowDynamic(height = 30, cols = 1)
    slider(min = 1, val = currentDaily, max = maxDaily, step = 1)
    setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.75.cfloat, 0.25])
    label(str = "Percent of profit from trades:")
    let newProfit: int = property2(name = "#", min = 0, val = currentProfit,
        max = 10, step = 1, incPerPixel = 1)
    if newProfit != currentProfit:
      currentProfit = newProfit
    setLayoutRowDynamic(height = 30, cols = 1)
    slider(min = 0, val = currentProfit, max = 10, step = 1)
    label(str = "Contract time:", alignment = centered)
    setLayoutRowDynamic(height = 35, cols = 1)
    let newContract = comboList(items = contractLength,
        selected = currentContract, itemHeight = 25, x = 200, y = 150)
    if newContract != currentContract:
      currentContract = newContract
    var newCost: int = recruit.price - ((currentDaily - recruit.payment) * 50) -
        (currentProfit * 5_000)
    newCost = case currentContract
      of 1:
        newCost - (recruit.price.float * 0.1).int
      of 2:
        newCost - (recruit.price.float * 0.5).int
      of 3:
        newCost - (recruit.price.float * 0.75).int
      of 4:
        newCost - (recruit.price.float * 0.9).int
      else:
        newCost
    if newCost < 1:
      newCost = 1
    cost = newCost
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk))
    except:
      dialog = setError(message = "Can't count price.")
      return
    hireText[1] = $cost
    try:
      hireWidth[1] = hireText[1].getTextWidth
    except:
      dialog = setError(message = "Can't get the width of the hire text.")
      return
    var canHire: bool = false
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    if moneyText.len == 1:
      colorLabel(str = moneyText[0], color = theme.colors[redColor])
    else:
      label(str = moneyText[0])
      colorLabel(str = moneyText[1], color = theme.colors[goldenColor])
      label(str = moneyText[2])
      if moneyAmount >= cost:
        canHire = true
    setLayoutRowStatic(height = 30, cols = 3, ratio = hireWidth)
    label(str = hireText[0])
    colorLabel(str = hireText[1], color = theme.colors[goldenColor])
    label(str = hireText[2])
    setLayoutRowDynamic(height = 30, cols = 2)
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    if canHire:
      imageLabelButton(image = images[negotiateColoredIcon], text = "Hire",
          alignment = right):
        dialog = none
        try:
          let contractLen: int = case currentContract
            of 1:
              100
            of 2:
              30
            of 3:
              20
            of 4:
              10
            else:
              -1
          hireRecruit(recruitIndex = recruitIndex, cost = cost,
              dailyPayment = currentDaily, tradePayment = currentProfit,
              contractLength = contractLen)
          setRecruits()
        except NoTraderError:
          dialog = setMessage(message = "You don't have a trader to hire the recruit.",
              title = "Can't hire the recruit.")
        except:
          dialog = setError(message = "Can't hire the recruit.")
    else:
      disabled:
        imageLabelButton(image = images[negotiateColoredIcon], text = "Hire",
            alignment = right):
          discard
    restoreButtonStyle()
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

proc setRecruitInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [],
    contractual.} =
  ## Set the data needed for show information about the selected recruit
  ##
  ## * data   - the index of the selected recruit
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
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
  # Show the last in-game messages
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
