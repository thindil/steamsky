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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to the information about the player's ship's crew
## members, like listing them, showing information, give orders, etc.

import std/[algorithm, sequtils, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crew, crewinventory, game, messages, shipscrew, shipscrew2, types]
import coreui, dialogs, errordialog, setui, table, themes, utilsui2

type
  CrewSortOrders = enum
    selectedAsc, selectedDesc, nameAsc, nameDesc, orderAsc, orderDesc, skillAsc,
      skillDesc, healthAsc, healthDesc, fatigueAsc, fatigueDesc, thirstAsc,
      thirstDesc, hungerAsc, hungerDesc, moraleAsc, moraleDesc, none
  LocalMemberData = object
    selected: bool
    name: string
    order: string
    skill: string
    health: SkillRange
    fatigue: int
    thirst: SkillRange
    hunger: SkillRange
    morale: SkillRange
    id: Natural

const defaultCrewSortOrder: CrewSortOrders = none

var
  showCrewOptions*: bool = false
    ## Show additonal options for managing the player's ship's crew
  skillIndex, currentOrder: Natural = 0
  crewSortOrder: CrewSortOrders = defaultCrewSortOrder
  availableOrdersText: seq[string] = @[]
  availableOrders: seq[CrewOrders] = @[]

proc sortMembers(x, y: LocalMemberData): int {.raises: [], tags: [],
    contractual.} =
  ## Compare two members and return which should go first, members on the sort
  ## order of the members
  ##
  ## * x - the first member to compare
  ## * y - the second member to compare
  ##
  ## Returns 1 if the first member should go first, -1 if the second member
  ## should go first.
  case crewSortOrder
  of selectedAsc:
    if x.selected < y.selected:
      return 1
    return -1
  of selectedDesc:
    if x.selected > y.selected:
      return 1
    return -1
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of orderAsc:
    if x.order < y.order:
      return 1
    return -1
  of orderDesc:
    if x.order > y.order:
      return 1
    return -1
  of skillAsc:
    if x.skill < y.skill:
      return 1
    return -1
  of skillDesc:
    if x.skill > y.skill:
      return 1
    return -1
  of healthAsc:
    if x.health < y.health:
      return 1
    return -1
  of healthDesc:
    if x.health > y.health:
      return 1
    return -1
  of fatigueAsc:
    if x.fatigue < y.fatigue:
      return 1
    return -1
  of fatigueDesc:
    if x.fatigue > y.fatigue:
      return 1
    return -1
  of thirstAsc:
    if x.thirst < y.thirst:
      return 1
    return -1
  of thirstDesc:
    if x.thirst > y.thirst:
      return 1
    return -1
  of hungerAsc:
    if x.hunger < y.hunger:
      return 1
    return -1
  of hungerDesc:
    if x.hunger > y.hunger:
      return 1
    return -1
  of moraleAsc:
    if x.morale < y.morale:
      return 1
    return -1
  of moraleDesc:
    if x.morale > y.morale:
      return 1
    return -1
  of none:
    return -1

proc getHighestSkill(memberIndex: Natural;
    dialog: var GameDialog): string {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Get the name of the highest skill of the selected crew member
  ##
  ## * memberIndex - the crew index of the member which the highest skill will
  ##                 be get
  ## * dialog -      the current in-game dialog displayed on the screen
  ##
  ## Returns the name of the highest skill of the selected crew member
  var
    highestLevel: Positive = 1
    highestIndex: Positive = 1
  for skill in playerShip.crew[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except KeyError:
    dialog = setError(message = "Can't thge the highest skill. Index: " & $highestIndex)
    return "Unknown"

proc sortCrew(sortAsc, sortDesc: CrewSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort the crew members on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if crewSortOrder == sortAsc:
    crewSortOrder = sortDesc
  else:
    crewSortOrder = sortAsc
  var localCrew: seq[LocalMemberData] = @[]
  for index, member in playerShip.crew:
    var selected: bool = false
    for data in crewDataList:
      if data.index == index:
        selected = data.checked
        break
    try:
      localCrew.add(y = LocalMemberData(selected: selected, name: member.name,
          order: $member.order, skill: (if skillIndex == 0: getHighestSkill(
          memberIndex = index, dialog = dialog) else: getSkillLevelName(
          skillLevel = getSkillLevel(
          member = member, skillIndex = skillIndex))), health: member.health,
          fatigue: member.tired - member.attributes[conditionIndex].level,
          thirst: member.thirst, hunger: member.hunger, morale: member.morale[
              1], id: index))
    except:
      dialog = setError(message = "Can't add local crew member.")
  localCrew.sort(cmp = sortMembers)
  crewDataList = @[]
  for member in localCrew:
    crewDataList.add(y = CrewData(index: member.id, checked: member.selected))

proc ordersForAll(order: CrewOrders; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Give the selected order to all crew members
  ##
  ## * order - the order to give
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  for i in playerShip.crew.low..playerShip.crew.high:
    try:
      giveOrders(ship = playerShip, memberIndex = i, givenOrder = order)
    except CrewOrderError:
      addMessage(message = getCurrentExceptionMsg(), mType = orderMessage)
    except:
      dialog = setError(message = "Can't give orders.")

proc setAvailableOrders*(memberIndex: Natural; dialog: var GameDialog)
  {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Set the list of available orders for the selected crew member
  ##
  ## * memberIndex - the index of the crew member for which the list will be set
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  availableOrdersText = @[]
  availableOrders = @[]
  var needRepair, needClean: bool = false
  for module in playerShip.modules:
    if module.durability < module.maxDurability:
      needRepair = true
    if (module.durability > 0 and module.mType == ModuleType2.cabin) and
        module.cleanliness < module.quality:
      needClean = true
    if needRepair and needClean:
      break
  let member: MemberData = playerShip.crew[memberIndex]
  if ((member.tired == 100 or member.hunger == 100 or member.thirst == 100) and
      member.order != rest) or member.skills.len == 0 or
      member.contractLength == 0:
    availableOrdersText.add(y = "Go on break")
    availableOrders.add(y = rest)
  else:
    if member.order != pilot:
      availableOrdersText.add(y = "Go piloting the ship")
      availableOrders.add(y = pilot)
    if member.order != engineer:
      availableOrdersText.add(y = "Go engineering the ship")
      availableOrders.add(y = engineer)

    proc isWorking(owners: seq[int]; mIndex: Natural): bool =
      for owner in owners:
        if owner == mIndex:
          return true
      return false

    var orderAdded: bool = false
    for index, module in playerShip.modules:
      if module.durability > 0:
        case module.mType
        of gun, harpoonGun:
          if module.owner[0] != memberIndex:
            availableOrdersText.add(y = "Operate " & module.name)
            availableOrders.add(y = gunner)
        of workshop:
          if not isWorking(owners = module.owner, mIndex = memberIndex) and
              module.craftingIndex.len > 0:
            try:
              availableOrdersText.add(y = (if module.craftingIndex.len > 6 and
                  module.craftingIndex[0..4] == "Study": "Study " & itemsList[
                  module.craftingIndex[
                  6..^1].strip.parseInt].name elif module.craftingIndex.len >
                  12 and module.craftingIndex[0..10] ==
                  "Deconstruct": "Deconstruct " & itemsList[
                  module.craftingIndex[
                  12..^1].strip.parseInt].name else: "Manufacture " &
                  $module.craftingAmount & "x " & itemsList[recipesList[
                  module.craftingIndex].resultIndex].name))
              availableOrders.add(y = craft)
            except:
              dialog = setError(message = "Can't add an available order.")
              return
        of cabin:
          if module.cleanliness < module.quality and member.order != clean and needClean:
            availableOrdersText.add(y = "Clean ship")
            availableOrders.add(y = clean)
            needClean = false
        of trainingRoom:
          if not isWorking(owners = module.owner, mIndex = memberIndex):
            availableOrdersText.add(y = "Go training in " & module.name)
            availableOrders.add(y = train)
        else:
          discard
        if needRepair and not orderAdded:
          availableOrdersText.add(y = "Repair ship")
          availableOrders.add(y = repair)
          orderAdded = true
    for index, member2 in playerShip.crew:
      if member2.health < 100 and index != memberIndex and member2.order != heal:
        availableOrdersText.add(y = "Heal wounded crew members")
        availableOrders.add(y = heal)
        break
    if playerShip.upgradeModule > -1 and member.order != upgrading:
      availableOrdersText.add(y = "Upgrade module")
      availableOrders.add(y = upgrading)
    if member.order != talk:
      availableOrdersText.add(y = "Talk with others")
      availableOrders.add(y = talk)
    if member.order != rest:
      availableOrdersText.add(y = "Go on break")
      availableOrders.add(y = rest)

proc setGiveOrder(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the dialog to give an order for the selected crew member
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  crewIndex = crewDataList[data].index
  dialog = giveOrderDialog
  currentOrder = 0
  setAvailableOrders(memberIndex = crewIndex, dialog = dialog)

proc showGiveOrder*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog to give an order for the selected crew member
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  const
    width: float = 400
    height: float = 200

  let
    member: MemberData = playerShip.crew[crewIndex]
    windowName: string = "Change order for " & member.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Current order:")
    try:
      colorLabel(str = getCurrentOrder(memberIndex = crewIndex),
          color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't get the current order.")
    label(str = "New order:")
    let newOrder: Natural = comboList(items = availableOrdersText,
        selected = currentOrder, itemHeight = 25, x = 200, y = 150)
    if newOrder != currentOrder:
      currentOrder = newOrder
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    imageLabelButton(image = images[giveOrderColoredIcon], text = "Assign",
        alignment = right):
      dialog = none
      try:
        giveOrders(ship = playerShip, memberIndex = crewIndex,
            givenOrder = availableOrders[currentOrder])
      except CrewOrderError:
        addMessage(message = getCurrentExceptionMsg(), mType = orderMessage)
      except:
        dialog = setError(message = "Can't give orders.")
    restoreButtonStyle()
    addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
        label = "Cancel", isPopup = false)

  windowSetFocus(name = windowName)

var
  currentTab: cint = 0
  tiredPoints: int = 0
  setPriorites: array[1..12, Natural] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  inventoryDataList*: seq[CrewData] = @[]
    ## The list of data related to the player's ship's crew members inventory
  spaceWidth*: array[2, cfloat] = [0.cfloat, 0]
    ## The width of the text with info about the crew member free inventory
    ## space
  spaceText*: array[2, string] = ["Free inventory space:", ""]
    ## The text with info about the player's ship's free cargo space

proc setInventoryInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog with information about the selected member's inventory
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  let member: MemberData = playerShip.crew[crewIndex]
  spaceText[1] = $freeInventory(memberIndex = crewIndex, amount = 0) & " kg"
  for index, text in spaceText:
    try:
      spaceWidth[index] = text.getTextWidth
    except:
      dialog = setError(message = "Can't get the width of the free space text.")
      return
  inventoryDataList = @[]
  for index in member.inventory.low..member.inventory.high:
    inventoryDataList.add(y = CrewData(index: index, checked: false))

proc setMemberInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the dialog with information about the selected member
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  crewIndex = crewDataList[data].index
  dialog = memberDialog
  currentTab = 0
  let member: MemberData = playerShip.crew[crewIndex]
  tiredPoints = member.tired - member.attributes[conditionIndex].level
  if tiredPoints < 0:
    tiredPoints = 0
  setPriorites = member.orders
  setInventoryInfo(dialog = dialog)
  setDialog(x = windowWidth / 5, y = windowHeight / 7)

proc showMemberInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog to give an order for the selected crew member
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  const
    width: float = 400
    height: float = 500

  let
    member: MemberData = playerShip.crew[crewIndex]
    windowName: string = member.name & "'s details"
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    changeStyle(field = spacing, x = 0, y = 0):
      changeStyle(field = buttonRounding, value = 0):
        var tabs: seq[string] = @[]
        if member.skills.len > 0 and member.contractLength != 0:
          setLayoutRowDynamic(height = 30, cols = 4)
          tabs = @["General", "Attributes", "Skills", "Priorites"]
        else:
          setLayoutRowDynamic(height = 30, cols = 1)
          tabs = @["General"]
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
      # General information about the selected crew member
      of 0:
        setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
        label(str = "Name:")
        colorLabel(str = member.name, color = theme.colors[goldenColor])
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Set a new name for the crew member")
        imageButton(image = images[editIcon]):
          dialog = renameMemberDialog
        if member.health < 100:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
          label(str = "Health:")
          if gameSettings.showNumbers:
            colorLabel(str = $member.health & "%", color = theme.colors[goldenColor])
          else:
            case member.health:
            of 81..99:
              colorLabel(str = "Slightly wounded", color = theme.colors[goldenColor])
            of 51..80:
              colorLabel(str = "Wounded", color = theme.colors[goldenColor])
            of 1..50:
              colorLabel(str = "Heavily wounded", color = theme.colors[goldenColor])
            else:
              discard
        if tiredPoints > 0:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
          label(str = "Tiredness:")
          if gameSettings.showNumbers:
            colorLabel(str = $tiredPoints & "%", color = theme.colors[goldenColor])
          else:
            case tiredPoints:
            of 1..40:
              colorLabel(str = "Bit tired", color = theme.colors[goldenColor])
            of 41..80:
              colorLabel(str = "Tired", color = theme.colors[goldenColor])
            of 81..99:
              colorLabel(str = "Very tired", color = theme.colors[goldenColor])
            of 100:
              colorLabel(str = "Unconscious", color = theme.colors[goldenColor])
            else:
              discard
        if member.thirst > 0:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
          label(str = "Thirst:")
          if gameSettings.showNumbers:
            colorLabel(str = $member.thirst & "%", color = theme.colors[goldenColor])
          else:
            case member.thirst:
            of 1..40:
              colorLabel(str = "Bit thirsty", color = theme.colors[goldenColor])
            of 41..80:
              colorLabel(str = "Thirsty", color = theme.colors[goldenColor])
            of 81..99:
              colorLabel(str = "Very thirsty", color = theme.colors[goldenColor])
            of 100:
              colorLabel(str = "Dehydrated", color = theme.colors[goldenColor])
            else:
              discard
        if member.hunger > 0:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
          label(str = "Hunger:")
          if gameSettings.showNumbers:
            colorLabel(str = $member.hunger & "%", color = theme.colors[goldenColor])
          else:
            case member.hunger:
            of 1..40:
              colorLabel(str = "Bit hungry", color = theme.colors[goldenColor])
            of 41..80:
              colorLabel(str = "Hungry", color = theme.colors[goldenColor])
            of 81..99:
              colorLabel(str = "Very hungry", color = theme.colors[goldenColor])
            of 100:
              colorLabel(str = "Starving", color = theme.colors[goldenColor])
            else:
              discard
        if member.morale[1] != 50:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
          label(str = "Morale:")
          if gameSettings.showNumbers:
            colorLabel(str = $member.morale[1] & "%", color = theme.colors[goldenColor])
          else:
            case member.morale[1]
            of 0..24:
              colorLabel(str = "Upset", color = theme.colors[goldenColor])
            of 25..49:
              colorLabel(str = "Unhappy", color = theme.colors[goldenColor])
            of 51..74:
              colorLabel(str = "Happy", color = theme.colors[goldenColor])
            of 75..100:
              colorLabel(str = "Excited", color = theme.colors[goldenColor])
            else:
              discard
        if member.skills.len > 0:
          setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
          label(str = "Order:")
          try:
            colorLabel(str = getCurrentOrder(memberIndex = crewIndex),
                color = theme.colors[goldenColor])
          except:
            dialog = setError(message = "Can't show the order info.")
            return
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Set a new order for the crew member")
          imageButton(image = images[giveOrderIcon]):
            dialog = giveOrderDialog
            setGiveOrder(data = crewIndex, dialog = dialog)
        let faction: FactionData = try:
            factionsList[member.faction]
          except:
            dialog = setError(message = "Can't get the crew member's faction.")
            return
        if "nogender" notin faction.flags:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
          label(str = "Gender:")
          colorLabel(str = (if member.gender == 'M': "Male" else: "Female"),
              color = theme.colors[goldenColor])
        setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
        label(str = "Faction:")
        colorLabel(str = faction.name, color = theme.colors[goldenColor])
        label(str = "Home base:")
        colorLabel(str = skyBases[member.homeBase].name, color = theme.colors[goldenColor])
        if member.skills.len == 0 or member.contractLength == 0:
          setLayoutRowDynamic(height = 35, cols = 1)
          label(str = "Passenger")
          if member.contractLength > 0:
            setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
            label(str = "Time limit:")
            var memberInfo: string = ""
            minutesToDate(minutes = member.contractLength,
                infoText = memberInfo)
            colorLabel(str = memberInfo, color = theme.colors[goldenColor])
        else:
          if crewIndex > 0:
            setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
            label(str = "Contract length:")
            colorLabel(str = (if member.contractLength >
                0: $member.contractLength & " days" else: "Pernament"),
                color = theme.colors[goldenColor])
            label(str = "Payment:")
            colorLabel(str = $member.payment[1] & " " & moneyName &
                " each day" & (if member.payment[2] > 0: " and " &
                $member.payment[2] &
                " percent of profit from each trade" else: ""),
                color = theme.colors[goldenColor])
      # Attributes of the selected crew member
      of 1:
        for index, attrib in member.attributes:
          setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
          label(str = attributesList[index].name & ":")
          colorLabel(str = getAttributeLevelName(attributeLevel = attrib.level),
              color = theme.colors[goldenColor])
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Show detailed information about the selected attribute.")
          imageButton(image = images[helpIcon]):
            let attribute: AttributeRecord = attributesList[index]
            dialog = setInfo(text = attribute.description,
                title = attribute.name)
          setLayoutRowDynamic(height = 20, cols = 1)
          var level: int = (if attrib.level > 2: attrib.level * 2 else: 6)
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "The current level of the attribute.")
          progressBar(value = level, maxValue = SkillRange.high,
              modifyable = false)
          setLayoutRowDynamic(height = 5, cols = 1)
          var exp: int = ((attrib.experience.float / (attrib.level.float *
              250.0)) * 100.0).int
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Experience need to reach the next level")
          progressBar(value = exp, maxValue = 100, modifyable = false)
      # Skills of the selected crew member
      of 2:
        for index, skill in member.skills:
          setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
          try:
            label(str = skillsList[skill.index].name & ":")
          except:
            dialog = setError(message = "Can't get the skill name.")
            return
          colorLabel(str = getSkillLevelName(skillLevel = skill.level),
              color = theme.colors[goldenColor])
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Show detailed information about the selected skill.")
          imageButton(image = images[helpIcon]):
            try:
              let skill: SkillRecord = skillsList[skill.index]
              dialog = setInfo(text = skill.description, title = skill.name)
            except:
              dialog = setError(message = "Can't get the skill info")
              return
          setLayoutRowDynamic(height = 20, cols = 1)
          var level: int = skill.level
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "The current level of the skill.")
          progressBar(value = level, maxValue = SkillRange.high,
              modifyable = false)
          setLayoutRowDynamic(height = 5, cols = 1)
          var exp: int = ((skill.experience.float / (skill.level.float *
              25.0)) * 100.0).int
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Experience need to reach the next level")
          progressBar(value = exp, maxValue = 100, modifyable = false)
      # Order priorites of the selected crew member
      of 3:
        setLayoutRowDynamic(height = 35, cols = 2)
        label(str = "Priority")
        label(str = "Level")
        const
          priorityLevels: array[3, string] = ["None", "Normal", "Highest"]
          prioritesNames: array[1..12, string] = ["Piloting:", "Engineering:",
              "Operating guns:", "Repair ship:", "Manufacturing:",
              "Upgrading ship:", "Talking in bases:", "Healing wounded:",
              "Cleaning ship:", "Defend ship:", "Board enemy ship", "Train skill:"]
        for index, priority in prioritesNames:
          label(str = priority)
          var newPriority: Natural = comboList(items = priorityLevels,
              selected = setPriorites[index], itemHeight = 25, x = 200, y = 150)
          if newPriority != setPriorites[index]:
            if newPriority == 2:
              for order in playerShip.crew[crewIndex].orders.mitems:
                if order == 2:
                  order = 1
                  break
              setPriorites = playerShip.crew[crewIndex].orders
            setPriorites[index] = newPriority
            playerShip.crew[crewIndex].orders[index] = newPriority
            try:
              updateOrders(ship = playerShip)
            except CrewOrderError, CrewNoSpaceError:
              dialog = setMessage(message = getCurrentExceptionMsg(),
                  title = "Can't give an order")
              return
            except:
              dialog = setError(message = "Can't update orders.")
              return
      else:
        discard
    setLayoutRowDynamic(height = 30, cols = (if playerShip.speed == docked and
        crewIndex > 0: 3 else: 2))
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show the crew member inventory")
    imageLabelButton(image = images[inventoryIcon], text = "Inventory",
        alignment = right):
      dialog = inventoryDialog
      setDialog(x = windowWidth / 9, y = windowHeight / 8)
    addCloseButton(dialog = dialog, isPopup = false)
    if playerShip.speed == docked and crewIndex > 0:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Remove the crew member from the ship's crew.")
      imageLabelButton(image = images[dismissIcon], text = "Dismiss",
          alignment = right):
        dialog = setQuestion(question = "Are you sure want to dismiss " &
            member.name & "?", qType = dismissMember, data = $crewIndex)

  windowSetFocus(name = windowName)

const
  headers: array[9, HeaderData[CrewSortOrders]] = [
    HeaderData[CrewSortOrders](label: "", sortAsc: selectedAsc,
        sortDesc: selectedDesc),
    HeaderData[CrewSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[CrewSortOrders](label: "Order", sortAsc: orderAsc,
        sortDesc: orderDesc),
    HeaderData[CrewSortOrders](label: "Skill", sortAsc: skillAsc,
        sortDesc: skillDesc),
    HeaderData[CrewSortOrders](label: "Health", sortAsc: healthAsc,
        sortDesc: healthDesc),
    HeaderData[CrewSortOrders](label: "Fatigue", sortAsc: fatigueAsc,
        sortDesc: fatigueDesc),
    HeaderData[CrewSortOrders](label: "Thirst", sortAsc: thirstAsc,
        sortDesc: thirstDesc),
    HeaderData[CrewSortOrders](label: "Hunger", sortAsc: hungerAsc,
        sortDesc: hungerDesc),
    HeaderData[CrewSortOrders](label: "Morale", sortAsc: moraleAsc,
        sortDesc: moraleDesc)]
  ratio: array[9, cfloat] = [40.cfloat, 300, 200, 200, 200, 200, 200, 200, 200]

proc showCrewInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the player's ship's crew members
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Show options related to managing the crew
  if showCrewOptions:
    var
      cols: Positive = 2
      ratio2: seq[cfloat] = @[0.4.cfloat, 0.1]
    if needClean:
      cols.inc
      ratio2.add(y = 0.1.cfloat)
    if needRepair:
      cols.inc
      ratio2.add(y = 0.1.cfloat)
    setLayoutRowDynamic(height = 35, cols = cols, ratio = ratio2)
    label(str = "Orders for all:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Go rest " &
        (if crewDataList.any(pred = proc (x: CrewData): bool = x.checked):
          "selected crew members" else: "everyone"))
    imageButton(image = images[goRestIcon]):
      ordersForAll(order = rest, dialog = dialog)
    if needClean:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Clean the ship everyone")
      imageButton(image = images[cleanOrderIcon]):
        ordersForAll(order = clean, dialog = dialog)
    if needRepair:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Repair the ship everyone")
      imageButton(image = images[repairOrderIcon]):
        ordersForAll(order = repair, dialog = dialog)
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show the level of the selected skill for the crew members.If selected option 'Highest', show the highest skill of the crew members.")
    label(str = "Skill:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show the level of the selected skill for the crew members.If selected option 'Highest', show the highest skill of the crew members.")
    let newSkill: Natural = comboList(items = crewSkillsList,
        selected = skillIndex, itemHeight = 25, x = 200, y = 150)
    if newSkill != skillIndex:
      skillIndex = newSkill
    setLayoutRowStatic(height = 35, cols = 2, width = 35)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Select all crew member")
    imageButton(image = images[selectAllIcon]):
      for data in crewDataList.mitems:
        data.checked = true
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Unselect all crew member")
    imageButton(image = images[unselectAllIcon]):
      for data in crewDataList.mitems:
        data.checked = false
  # Show the list of crew members
  addHeader(headers = headers, ratio = ratio, tooltip = "crew members",
      code = sortCrew, dialog = dialog)
  var currentRow: Positive = 1
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
  let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
  var row: Positive = 1
  for index, data in crewDataList.mpairs:
    if currentRow < startRow:
      currentRow.inc
      continue
    addCheckButton(tooltip = "Select the crew member to give orders to them.",
        checked = data.checked)
    addButton(label = playerShip.crew[data.index].name,
        tooltip = "Show available crew member's options", data = data.index,
        code = setMemberInfo, dialog = dialog)
    addButton(label = ($playerShip.crew[data.index].order).capitalizeAscii,
        tooltip = "The current order for the selected crew member. Press the mouse button to change it.",
        data = data.index, code = setGiveOrder, dialog = dialog)
    if skillIndex == 0:
      addButton(label = getHighestSkill(memberIndex = data.index,
          dialog = dialog),
          tooltip = "The highest skill of the selected crew member",
          data = data.index, code = setMemberInfo, dialog = dialog)
    else:
      try:
        addButton(label = getSkillLevelName(skillLevel = getSkillLevel(
            member = playerShip.crew[data.index], skillIndex = findSkillIndex(
            skillName = crewSkillsList[skillIndex]))),
            tooltip = "The level of " & crewSkillsList[skillIndex] &
            " of the selected crew member", data = data.index,
            code = setMemberInfo, dialog = dialog)
      except KeyError:
        dialog = setError(message = "Can't get the level of the skill.")
    addProgressBar(tooltip = "The current health level of the selected crew member",
        value = playerShip.crew[data.index].health, maxValue = SkillRange.high,
        data = data.index, code = setMemberInfo, dialog = dialog)
    var tiredLevel: int = playerShip.crew[data.index].tired - playerShip.crew[
        data.index].attributes[conditionIndex].level
    if tiredLevel < 0:
      tiredLevel = 0
    addProgressBar(tooltip = "The current tired level of the selected crew member",
        value = tiredLevel, maxValue = SkillRange.high, data = data.index,
        code = setMemberInfo, dialog = dialog)
    addProgressBar(tooltip = "The current thirst level of the selected crew member",
        value = playerShip.crew[data.index].thirst, maxValue = SkillRange.high,
        data = data.index, code = setMemberInfo, dialog = dialog)
    addProgressBar(tooltip = "The current hunger level of the selected crew member",
        value = playerShip.crew[data.index].hunger, maxValue = SkillRange.high,
        data = data.index, code = setMemberInfo, dialog = dialog)
    addProgressBar(tooltip = "The current morale level of the selected crew member",
        value = playerShip.crew[data.index].morale[1],
            maxValue = SkillRange.high,
        data = data.index, code = setMemberInfo, dialog = dialog)
    row.inc
    if row == gameSettings.listsLimit + 1:
      break
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
