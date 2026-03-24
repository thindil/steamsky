# Copyright 2026 Bartek thindil Jasicki
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

## Provides code related to showing debug interface.

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basescargo, basestypes, events, game, gamesaveload, items, maps,
    shipscargo, types]
import coreui, errordialog, themes

type
  AttributeData = object
    name: string
    value: SkillRange
  DebugDialogs = enum
    none, addItem, updateItem, setBase, setShip, setBaseEvent
  DebugTabs = enum
    ship, crew, cargo, bases, world

const
  itemQualities: array[5, string] = ["Poor", "Low", "Normal", "Good", "Excellent"]

var
  shipX, shipY, weight, maxDurability, itemAmount, cargoAmount, ship2X, ship2Y,
    shipDuration: Positive = 1
  playerModules, protoModules, crewList, availableSkills, itemsNames,
    cargoNames, basesNames, basesTypesNames, ownersNames, shipsNames: seq[
      string] = @[]
  moduleSelected, protoSelected, durability, upgradeProgress, crewSelected,
    skillSelected, itemQuality, itemSelected, cargoSelected, cargoQuality,
    baseSelected, baseTypeSelected, ownerSelected, sizeSelected, population,
    money, shipSelected, base2Selected, item2Selected: Natural = 0
  memberProperties: array[6, Natural] = [0, 0, 0, 0, 0, 0]
  memberAttribs, memberSkills: seq[AttributeData] = @[]
  itemName, cargoName, baseName, shipName, base2Name: string = ""
  debugDialog: DebugDialogs = none
  debugTab: DebugTabs = ship
  reputation: ReputationRange = 0

proc setSelectedModule() {.raises: [], tags: [], contractual.} =
  ## Set the data of the selected module in the player's ship
  var mIndex: Natural = 0
  let protoIndex: Natural = playerShip.modules[moduleSelected].protoIndex
  for index, module in modulesList:
    protoModules.add(y = module.name)
    if protoIndex == index:
      protoSelected = mIndex
    mIndex.inc
  if playerShip.modules[moduleSelected].weight > 0:
    weight = playerShip.modules[moduleSelected].weight
  else:
    try:
      weight = modulesList[protoIndex].weight
    except:
      echo getCurrentExceptionMsg()
  durability = playerShip.modules[moduleSelected].durability
  maxDurability = playerShip.modules[moduleSelected].maxDurability
  upgradeProgress = playerShip.modules[moduleSelected].upgradeProgress

proc setAvailableSkills() {.raises: [], tags: [], contractual.} =
  ## Set the list of available skills for the selected crew member
  availableSkills = @[]
  for skill in skillsList.values:
    var added: bool = false
    for mSkill in memberSkills:
      if skill.name == mSkill.name:
        added = true
        break
    if not added:
      availableSkills.add(y = skill.name)
  skillSelected = 0

proc setSelectedMember() {.raises: [], tags: [], contractual.} =
  ## Set the data of the selected member in the player's ship's crew
  let member: MemberData = playerShip.crew[crewSelected]
  memberProperties = [member.health.Natural, member.thirst.Natural,
      member.hunger.Natural, member.tired.Natural, member.morale[1],
      member.loyalty.Natural]
  memberAttribs = @[]
  for index, attrib in member.attributes:
    memberAttribs.add(y = AttributeData(name: attributesList[index].name,
        value: attrib.level))
  memberSkills = @[]
  for index, skill in member.skills:
    try:
      memberSkills.add(y = AttributeData(name: skillsList[skill.index].name,
          value: skill.level))
    except:
      echo getCurrentExceptionMsg()
  setAvailableSkills()

proc setCargoData() {.raises: [], tags: [], contractual.} =
  ## Set the data of the player's ship's cargo
  cargoNames = @[]
  for item in playerShip.cargo:
    cargoNames.add(y = getItemName(item = item, damageInfo = false,
        toLower = false))
  cargoSelected = 0
  let item: InventoryData = playerShip.cargo[cargoSelected]
  cargoName = getItemName(item = item, damageInfo = false, toLower = false)
  cargoAmount = item.amount
  cargoQuality = item.quality.ord

proc setDebugData*() {.raises: [], tags: [], contractual.} =
  ## Set the data for the debug UI
  shipX = playerShip.skyX
  shipY = playerShip.skyY
  playerModules = @[]
  for module in playerShip.modules:
    playerModules.add(y = module.name)
  moduleSelected = 0
  protoModules = @[]
  setSelectedModule()
  crewList = @[]
  for member in playerShip.crew:
    crewList.add(y = member.name)
  crewSelected = 0
  setSelectedMember()
  itemsNames = @[]
  for item in itemsList.values:
    itemsNames.add(y = item.name)
  itemName = ""
  itemAmount = 1
  itemQuality = 2
  itemSelected = 0
  setCargoData()
  basesNames = @[]
  for base in skyBases:
    basesNames.add(y = base.name)
  baseName = ""
  baseSelected = 0
  basesTypesNames = @[]
  for baseType in basesTypesList.values:
    basesTypesNames.add(y = baseType.name)
  baseTypeSelected = 0
  ownersNames = @[]
  for faction in factionsList.values:
    ownersNames.add(y = faction.name)
  ownerSelected = 0
  sizeSelected = 0
  population = 0
  reputation = 0
  money = 0
  for ship in protoShipsList.values:
    shipsNames.add(y = ship.name)
  shipName = ""
  shipSelected = 0
  ship2X = 1
  ship2Y = 1
  shipDuration = 60
  base2Selected = 0
  item2Selected = 0
  base2Name = ""

proc showShipTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes in the player's ship
  setLayoutRowDynamic(height = 30, cols = 5)
  labelButton(title = "Move ship"):
    playerShip.skyX = shipX
    playerShip.skyY = shipY
  label(str = "X:")
  shipX = property2(name = "#", min = 1, val = shipX, max = 1_024, step = 1,
      incPerPixel = 1)
  label(str = "Y:")
  shipY = property2(name = "#", min = 1, val = shipY, max = 1_024, step = 1,
      incPerPixel = 1)
  setLayoutRowDynamic(height = 30, cols = 2)
  label(str = "Module:")
  let newModule = comboList(items = playerModules, selected = moduleSelected,
      itemHeight = 25, x = 235, y = 125)
  if newModule != moduleSelected:
    moduleSelected = newModule
    setSelectedModule()
  label(str = "Prototype:")
  protoSelected = comboList(items = protoModules, selected = protoSelected,
      itemHeight = 25, x = 235, y = 125)
  label(str = "Weight:")
  weight = property2(name = "#", min = 1, val = weight, max = 1_00_000,
      step = 1, incPerPixel = 1)
  label(str = "Durability:")
  durability = property2(name = "#", min = 1, val = durability,
      max = maxDurability, step = 1, incPerPixel = 1)
  label(str = "Max durability:")
  maxDurability = property2(name = "#", min = 1, val = maxDurability,
      max = 1_000, step = 1, incPerPixel = 1)
  label(str = "Upgrade progress:")
  upgradeProgress = property2(name = "#", min = 1, val = upgradeProgress,
      max = 100, step = 1, incPerPixel = 1)
  setLayoutRowDynamic(height = 30, cols = 1)
  labelButton(title = "Change"):
    for index, module in modulesList:
      if protoModules[protoSelected] == module.name:
        playerShip.modules[moduleSelected].protoIndex = index
        break
    playerShip.modules[moduleSelected].weight = weight
    playerShip.modules[moduleSelected].durability = durability
    playerShip.modules[moduleSelected].maxDurability = maxDurability
    playerShip.modules[moduleSelected].upgradeProgress = upgradeProgress

proc showCrewTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes to the player's ship's crew members
  setLayoutRowDynamic(height = 30, cols = 2)
  label(str = "Member:")
  let newMember = comboList(items = crewList, selected = crewSelected,
      itemHeight = 25, x = 235, y = 125)
  if newMember != crewSelected:
    crewSelected = newMember
    setSelectedMember()
  setLayoutRowDynamic(height = 370, cols = 3)
  group(title = "memberProperties", flags = {windowNoScrollbar}):
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Health")
    memberProperties[0] = property2(name = "#", min = 1, val = memberProperties[
        0], max = 100, step = 1, incPerPixel = 1)
    const labelTexts: array[1..5, string] = ["Thirst", "Hunger", "Tired",
        "Morale", "Loyalty"]
    for i in 1..5:
      label(str = labelTexts[i])
      memberProperties[i] = property2(name = "#", min = 0,
          val = memberProperties[i], max = 100, step = 1, incPerPixel = 1)
  group(title = "memberAttribs", flags = {windowNoScrollbar}):
    setLayoutRowDynamic(height = 30, cols = 2)
    for index, attrib in memberAttribs:
      label(str = attrib.name)
      let newVal: SkillRange = property2(name = "#", min = 1,
          val = attrib.value, max = 100, step = 1, incPerPixel = 1)
      if newVal != attrib.value:
        memberAttribs[index].value = newVal
  group(title = "memberSkills", flags = {windowNoScrollbar}):
    setLayoutRowDynamic(height = 30, cols = 2)
    for index, skill in memberSkills:
      label(str = skill.name)
      let newVal: SkillRange = property2(name = "#", min = 1,
          val = skill.value, max = 100, step = 1, incPerPixel = 1)
      if newVal != skill.value:
        memberSkills[index].value = newVal
  setLayoutRowDynamic(height = 30, cols = 3)
  labelButton(title = "Change"):
    playerShip.crew[crewSelected].health = memberProperties[0]
    playerShip.crew[crewSelected].thirst = memberProperties[1]
    playerShip.crew[crewSelected].hunger = memberProperties[2]
    playerShip.crew[crewSelected].tired = memberProperties[3]
    playerShip.crew[crewSelected].morale[1] = memberProperties[4]
    playerShip.crew[crewSelected].loyalty = memberProperties[5]
    playerShip.crew[crewSelected].attributes = @[]
    for attrib in memberAttribs:
      playerShip.crew[crewSelected].attributes.add(y = MobAttributeRecord(
          level: attrib.value, experience: 0))
    playerShip.crew[crewSelected].skills = @[]
    for skill in memberSkills:
      for index, pSkill in skillsList:
        if pSkill.name == skill.name:
          playerShip.crew[crewSelected].skills.add(y = SkillInfo(index: index,
              level: skill.value, experience: 0))
          break
  labelButton(title = "Add skill"):
    memberSkills.add(y = AttributeData(name: availableSkills[skillSelected],
        value: 1))
    setAvailableSkills()
  skillSelected = comboList(items = availableSkills, selected = skillSelected,
      itemHeight = 25, x = 235, y = 125)

proc showCargoTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes to the player's ship's cargo
  setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.2.cfloat, 0.7, 0.1])
  labelButton(title = "Add:"):
    try:
      var protoIndex: Positive = 1
      for index, item in itemsList:
        if item.name == itemsNames[itemSelected]:
          protoIndex = index
          break
      updateCargo(ship = playerShip, protoIndex = protoIndex,
          amount = itemAmount, quality = itemQuality.ObjectQuality,
          breakChance = itemsList[protoIndex].breakChance)
      setCargoData()
    except:
      discard
  editString(text = itemName, maxLen = 64)
  imageButton(image = images[assignCrewIcon]):
    debugDialog = addItem
  setLayoutRowDynamic(height = 30, cols = 2)
  label(str = "Amount:")
  itemAmount = property2(name = "#", min = 1, val = itemAmount, max = 1_000_000,
      step = 1, incPerPixel = 1)
  label(str = "Quality:")
  itemQuality = comboList(items = itemQualities, selected = itemQuality,
      itemHeight = 25, x = 235, y = 125)
  setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.2.cfloat, 0.7, 0.1])
  labelButton(title = "Update:"):
    playerShip.cargo[cargoSelected].amount = cargoAmount
    playerShip.cargo[cargoSelected].quality = cargoQuality.ObjectQuality
  editString(text = cargoName, maxLen = 64)
  imageButton(image = images[assignCrewIcon]):
    debugDialog = updateItem
  setLayoutRowDynamic(height = 30, cols = 2)
  label(str = "Amount:")
  cargoAmount = property2(name = "#", min = 1, val = cargoAmount,
      max = 1_000_000, step = 1, incPerPixel = 1)
  label(str = "Quality:")
  cargoQuality = comboList(items = itemQualities, selected = cargoQuality,
      itemHeight = 25, x = 235, y = 125)

proc showAddItemDialog() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the dialog with list of items which can be added to the player's
  ## ship's cargo
  window(name = "Item to add", x = 300, y = 100, w = 300, h = 120, flags = {
      windowBorder, windowTitle}):
    setLayoutRowDynamic(height = 25, cols = 1)
    itemSelected = comboList(items = itemsNames, selected = itemSelected,
        itemHeight = 25, x = 290, y = 200)
    labelButton(title = "Add"):
      itemName = itemsNames[itemSelected]
      debugDialog = none

  windowSetFocus(name = "Item to add")

proc showUpdateItemDialog() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the dialog with list of items which can be updated in the player's
  ## ship's cargo
  window(name = "Item to update", x = 300, y = 100, w = 300, h = 120, flags = {
      windowBorder, windowTitle}):
    setLayoutRowDynamic(height = 25, cols = 1)
    cargoSelected = comboList(items = cargoNames, selected = cargoSelected,
        itemHeight = 25, x = 290, y = 200)
    labelButton(title = "Update"):
      cargoName = cargoNames[cargoSelected]
      let item = playerShip.cargo[cargoSelected]
      cargoAmount = item.amount
      cargoQuality = item.quality.ord
      debugDialog = none

  windowSetFocus(name = "Item to update")

proc showBasesTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes to the sky bases
  setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.2.cfloat, 0.7, 0.1])
  label(str = "Base:")
  editString(text = baseName, maxLen = 64)
  imageButton(image = images[assignCrewIcon]):
    debugDialog = setBase
  setLayoutRowDynamic(height = 30, cols = 2)
  label(str = "Type:")
  baseTypeSelected = comboList(items = basesTypesNames,
      selected = baseTypeSelected, itemHeight = 25, x = 290, y = 200)
  label(str = "Owner:")
  ownerSelected = comboList(items = ownersNames, selected = ownerSelected,
      itemHeight = 25, x = 290, y = 200)
  const sizesNames: array[3, string] = ["Small", "Medium", "Big"]
  label(str = "Size:")
  sizeSelected = comboList(items = sizesNames, selected = sizeSelected,
      itemHeight = 25, x = 290, y = 200)
  label(str = "Population:")
  population = property2(name = "#", min = 0, val = population, max = 10_000,
      step = 1, incPerPixel = 1)
  label(str = "Reputation:")
  reputation = property2(name = "#", min = -100, val = reputation, max = 100,
      step = 1, incPerPixel = 1)
  label(str = "Money:")
  money = property2(name = "#", min = 0, val = money, max = 1_000_000, step = 1,
      incPerPixel = 1)
  setLayoutRowDynamic(height = 30, cols = 1)
  labelButton(title = "Update"):
    let baseIndex: BasesRange = baseSelected + 1
    skyBases[baseIndex].baseType = $baseTypeSelected
    var index: Natural = 0
    for i in factionsList.keys:
      if index == ownerSelected:
        skyBases[baseIndex].owner = i
        break
      index.inc
    skyBases[baseIndex].size = sizeSelected.BasesSize
    skyBases[baseIndex].population = population
    skyBases[baseIndex].reputation.level = reputation
    try:
      updateBaseCargo(protoIndex = moneyIndex, amount = money, quality = normal,
          breakChance = -1)
    except:
      discard

proc showSetBaseDialog() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the dialog with list of bases which can be set
  window(name = "Bases", x = 300, y = 100, w = 300, h = 120, flags = {
      windowBorder, windowTitle}):
    setLayoutRowDynamic(height = 25, cols = 1)
    baseSelected = comboList(items = basesNames, selected = baseSelected,
        itemHeight = 25, x = 290, y = 200)
    labelButton(title = "Select"):
      baseName = basesNames[baseSelected]
      let base: BaseRecord = skyBases[baseSelected + 1]
      try:
        baseTypeSelected = base.baseType.parseInt
      except:
        discard
      var index: Natural = 0
      for i in factionsList.keys:
        if i == base.owner:
          ownerSelected = index
          break
        index.inc
      sizeSelected = base.size.ord
      population = base.population
      reputation = base.reputation.level
      if base.cargo.len > 0:
        money = base.cargo[0].amount
      else:
        money = 0
      debugDialog = none

proc showWorldTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes to the world's events
  setLayoutRowDynamic(height = 370, cols = 2)
  group(title = "shipProperties", flags = {windowNoScrollbar}):
    setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.2.cfloat, 0.6, 0.2])
    label(str = "Ship:")
    editString(text = shipName, maxLen = 64)
    imageButton(image = images[assignCrewIcon]):
      debugDialog = setShip
    setLayoutRowDynamic(height = 25, cols = 2)
    label(str = "X:")
    ship2X = property2(name = "#", min = 1, val = ship2X, max = 1_024, step = 1,
        incPerPixel = 1)
    label(str = "Y:")
    ship2Y = property2(name = "#", min = 1, val = ship2Y, max = 1_024, step = 1,
        incPerPixel = 1)
    label(str = "Duration:")
    shipDuration = property2(name = "#", min = 60, val = shipDuration,
        max = 1_000, step = 1, incPerPixel = 1)
    setLayoutRowDynamic(height = 25, cols = 1)
    labelButton(title = "Add ship"):
      var friendlyShips: seq[Positive] = @[]
      try:
        generateFriendlyShips(ships = friendlyShips)
      except:
        discard
      var traders: seq[Positive] = @[]
      generateTraders(ships = traders)
      for index, ship in protoShipsList:
        if ship.name == shipName:
          if index in traders:
            eventsList.add(y = EventData(skyX: ship2X, skyY: ship2Y,
                time: shipDuration, eType: trader, shipIndex: index))
          elif index in friendlyShips:
            eventsList.add(y = EventData(skyX: ship2X, skyY: ship2Y,
                time: shipDuration, eType: friendlyShip, shipIndex: index))
          else:
            eventsList.add(y = EventData(skyX: ship2X, skyY: ship2Y,
                time: shipDuration, eType: enemyShip, shipIndex: index))
          skyMap[ship2X][ship2Y].eventIndex = eventsList.high
  group(title = "baseProperties", flags = {windowNoScrollbar}):
    setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.2.cfloat, 0.6, 0.2])
    label(str = "Base:")
    editString(text = base2Name, maxLen = 64)
    imageButton(image = images[assignCrewIcon]):
      debugDialog = setBaseEvent

proc showSetShipDialog() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the dialog with list of proto ships which can be set
  window(name = "Ships", x = 300, y = 100, w = 300, h = 120, flags = {
      windowBorder, windowTitle}):
    setLayoutRowDynamic(height = 25, cols = 1)
    shipSelected = comboList(items = shipsNames, selected = shipSelected,
        itemHeight = 25, x = 290, y = 200)
    labelButton(title = "Select"):
      shipName = shipsNames[shipSelected]
      debugDialog = none

proc showSetBaseEventDialog() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the dialog with list of bases for an event
  window(name = "Bases", x = 300, y = 100, w = 300, h = 120, flags = {
      windowBorder, windowTitle}):
    setLayoutRowDynamic(height = 25, cols = 1)
    base2Selected = comboList(items = basesNames, selected = base2Selected,
        itemHeight = 25, x = 290, y = 200)
    labelButton(title = "Select"):
      base2Name = basesNames[base2Selected]
      debugDialog = none

proc showDebugUI*(dialog: var GameDialog) {.raises: [], tags: [ReadIOEffect,
    RootEffect], contractual.} =
  ## Show the debug dialog with various development options
  ##
  ## * dialog - the current in-game dialog to show
  ##
  ## Returns parameter dialog.
  const
    width: float = 700
    height: float = 500
    groupHeight: float = height - 40
    groupOneWidth: float = width * 0.25
    groupTwoWidth: float = width * 0.74
  updateDialog(width = width, height = height)
  window(name = "Debug options", x = 40, y = 0, w = width, h = height, flags = {
      windowBorder, windowTitle, windowMinimizable, windowMovable,
      windowNoScrollbar}):
    layoutSpaceStatic(height = groupHeight, widgetsCount = 2):
      row(x = 0, y = 0, w = groupOneWidth, h = groupHeight):
        group(title = "debugButtons", flags = {windowNoScrollbar}):
          setLayoutRowDynamic(height = 30, cols = 1)
          labelButton(title = "Ship"):
            debugTab = ship
          labelButton(title = "Crew"):
            debugTab = crew
          labelButton(title = "Cargo"):
            debugTab = cargo
          labelButton(title = "Bases"):
            debugTab = bases
          labelButton(title = "World"):
            debugTab = world
          labelButton(title = "Refresh"):
            setDebugData()
          labelButton(title = "Save game"):
            try:
              saveGame(prettyPrint = true)
            except:
              dialog = setError(message = "Can't save the game")
      row(x = groupOneWidth, y = 0, w = groupTwoWidth, h = groupHeight):
        group(title = "debugMenus", flags = {windowNoFlags}):
          case debugTab
          of ship:
            showShipTab()
          of crew:
            showCrewTab()
          of cargo:
            showCargoTab()
          of bases:
            showBasesTab()
          of world:
            showWorldTab()
  case debugDialog
  of addItem:
    showAddItemDialog()
  of updateItem:
    showUpdateItemDialog()
  of setBase:
    showSetBaseDialog()
  of setShip:
    showSetShipDialog()
  of setBaseEvent:
    showSetBaseEventDialog()
  of none:
    windowSetFocus(name = "Debug options")
