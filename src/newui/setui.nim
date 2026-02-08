# Copyright 2025-2026 Bartek thindil Jasicki
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

## Provides code related to training skills in bases, like show the UI,
## set the skill to train, etc.

import std/[strutils, strformat, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basescargo, basesship, basestrade, basestypes, combat, config,
    crafts, crew, game, goals, help, items, maps, missions, reputation,
    shipscargo, shipscrew, statistics, stories, types]
import coreui, errordialog, utilsui2, themes

var
  moneyAmount*: Natural = 0
    ## The amount of money in the player's ship's cargo
  moneyText*: seq[string] = @[]
    ## The text with information about money in player's ship's cargo and trader
  moneyWidth*: seq[cfloat] = @[]
    ## The width in pixels of the text with information about money
  cargoWidth*: array[4, cfloat] = [0.cfloat, 0, 0, 0]
    ## The width of the text with info about the player's ship's free cargo
    ## space
  cargoText*: array[4, string] = ["Free cargo space is ", "", "Base has ", ""]
    ## The text with info about the player's ship's free cargo space

proc setMoneyText(action: string; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the text about money owned by the player and its width
  ##
  ## * action - the action for which money are needed
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  moneyAmount = moneyAmount(inventory = playerShip.cargo)
  moneyText = @[]
  moneyWidth = @[]
  if moneyAmount == 0:
    moneyText.add(y = "You don't have " & moneyName & action)
  else:
    moneyText.add(y = "You have ")
    moneyText.add(y = $moneyAmount & " " & moneyName)
  for text in moneyText:
    try:
      moneyWidth.add(y = text.getTextWidth)
    except:
      dialog = setError(message = "Can't get the width of the money text.")

proc setCargoText(baseIndex: ExtendedBasesRange;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Set the text about free cargo space in the player's ship and a base and
  ## their width
  ##
  ## * action - the action for which money are needed
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  var freeSpace: int = try:
      freeCargo(amount = 0)
    except:
      dialog = setError(message = "Can't get free space.")
      return
  if freeSpace < 0:
    freeSpace = 0
  cargoText[1] = $freeSpace & " kg"
  if baseIndex > 0:
    cargoText[3] = $countFreeCargo(baseIndex = baseIndex) & " free space"
  else:
    cargoText[3] = "128 free space"
  for index, text in cargoText:
    try:
      cargoWidth[index] = text.getTextWidth
    except:
      dialog = setError(message = "Can't get the width of the money text.")
      return

#######################
# Setting the school UI
#######################

var
  crewList*: seq[string] = @[]
    ## The list of names of the player's ship's crew members
  schoolSkillsList*: seq[string] = @[]
    ## The list of skills which the crew member can learn, with prices
  crewIndex*: Natural = 0
    ## The index of currently selected crew member
  skillIndex*: Natural = 0
    ## The index of currently selected skill
  minCost*: Natural = 0
    ## The minimum cost of training
  maxCost*: Natural = 0
    ## The maximum cost of training
  amount*: Positive = 1
    ## The amount of training sessions
  timesCost*: Positive = 1
    ## The cost of all training
  oneTrainCost*: Positive = 1
    ## The cost of one training session
  skillsIndexes*: seq[Natural] = @[]
    ## The list of indexes of skills of the selected crew member

proc setSchoolSkills*(){.raises: [], tags: [], contractual.} =
  ## Set the skills list for the selected crew member
  schoolSkillsList = @[]
  for index, skill in skillsList:
    var skillLevel: SkillRange = 0
    for skill2 in playerShip.crew[crewIndex].skills:
      if skill2.index == index:
        skillLevel = skill2.level
        break
    if skillLevel != 100:
      schoolSkillsList.add(y = skill.name & ": " & (if skillLevel ==
          0: "Untrained" else: getSkillLevelName(
          skillLevel = skillLevel).strip))
      skillsIndexes.add(y = index)

proc setTrainingCost*(dialog: var GameDialog){.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the one training session cost for the selected skill of the selected
  ## crew member
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  oneTrainCost = try:
      trainCost(memberIndex = crewIndex, skillIndex = skillsIndexes[skillIndex],
          traderIndex = findMember(order = talk))
    except:
      dialog = setError(message = "Can't count the training cost.")
      return
  timesCost = oneTrainCost * amount
  minCost = oneTrainCost
  if moneyAmount < 1:
    minCost = 0
    maxCost = 0
  else:
    maxCost = moneyAmount

proc setSchool*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for school UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  setMoneyText(action = " to pay for learning", dialog = dialog)
  crewList = @[]
  for member in playerShip.crew:
    crewList.add(y = member.name)
  setSchoolSkills()
  setTrainingCost(dialog = dialog)

#########################
# Setting the recruits UI
#########################

var
  currentPage*: Positive = 1
    ## The current page in the table
  recruitsIndexes*: seq[Natural] = @[]
    ## The indexex of the recruits in the base
  baseIndex*: ExtendedBasesRange = 0
    ## The index of the base

proc setRecruits*() {.raises: [], tags: [RootEffect], contractual.} =
  ## Set the data for recruits UI
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if recruitsIndexes.len != skyBases[baseIndex].recruits.len:
    recruitsIndexes = @[]
    for index, _ in skyBases[baseIndex].recruits:
      recruitsIndexes.add(y = index)
  currentPage = 1

#######################
# Setting the combat UI
#######################

var
  pilotList*: seq[string] = @["Nobody"]
    ## The list of crew members who can take pilot position with information
    ## their piloting skill's level
  engineerList*: seq[string] = @["Nobody"]
    ## The list of crew members who can take engineer position with information
    ## their engineering skill's level
  gunnerList*: seq[string] = @["Nobody"]
    ## The list of crew members who can take gunner position with information
    ## their gunnery skill's level
  pilotIndex*: Natural = 0
    ## The index in the player's ship's crew of the member who is set as pilot
  engineerIndex*: Natural = 0
    ## The index in the player's ship's crew of the member who is set as
    ## engineer
  gunnersIndex*: seq[Natural] = @[]
    ## The list of indexes in the player's ship's crew of members who are set
    ## as gunners
  boardingParty*: seq[bool] = @[]
    ## The list of boarding party members. If true, the selected player's
    ## ship's crew member is in the boarding party
  defenders*: seq[bool] = @[]
    ## The list of the player's ship's defenders. If true, the selected
    ## player's ship's crew member defeding the ship.

proc updateCrewLists*() {.raises: [], tags: [RootEffect], contractual.} =
  ## Update the list of available crew members for all positions in combat
  pilotList = @["Nobody"]
  engineerList = @["Nobody"]
  for index, member in playerShip.crew:
    if member.skills.len > 0:
      pilotList.add(y = member.name & getSkillMarks(skillIndex = pilotingSkill,
          memberIndex = index))
      engineerList.add(y = member.name & getSkillMarks(
          skillIndex = engineeringSkill, memberIndex = index))
      gunnerList.add(y = member.name & getSkillMarks(skillIndex = gunnerySkill,
          memberIndex = index))

proc updateParties*() {.raises: [], tags: [], contractual.} =
  ## Update boarding party and defenders lists
  boardingParty = @[]
  defenders = @[]
  for member in playerShip.crew:
    boardingParty.add(y = member.order == boarding)
    defenders.add(y = member.order == defend)

proc setCombat*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the combat UI and combat itself
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  try:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and
        enemyName != protoShipsList[eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex].name:
      let combatStarted: bool = startCombat(enemyIndex = eventsList[skyMap[
          playerShip.skyX][playerShip.skyY].eventIndex].shipIndex,
          newCombat = false)
      if not combatStarted:
        return
  except:
    dialog = setError(message = "Can't start the combat.")
    return
  state = combat
  inCombat = true
  dialog = none
  engineerOrder = 3
  pilotIndex = findMember(order = pilot) + 1
  engineerIndex = findMember(order = engineer) + 1
  gunnersIndex = @[]
  updateParties()
  for gun in guns:
    gunnersIndex.add(y = playerShip.modules[gun[1]].owner[0] + 1)
  updateCrewLists()

#######################
# Setting the trades UI
#######################

type
  ItemsSortOrders* = enum
    ## Sorting order of the list of items to trade
    nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc,
      qualityAsc, qualityDesc, priceAsc, priceDesc, profitAsc, profitDesc,
      weightAsc, weightDesc, ownedAsc, ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder*: ItemsSortOrders = none
  ## Default sorting order for list of items to trade

var
  itemsSortOrder*: ItemsSortOrders = defaultItemsSortOrder
    ## The current sorting order of the list of items for trade
  itemsIndexes*: seq[int] = @[]
    ## The list of indexes of items for trade
  baseCargo*: seq[BaseCargo] = @[]
    ## The list of item to trade in the base's cargo
  typesList*: seq[string] = @["All"]
    ## The list of items' types
  nameSearch*: string = ""
    ## The string to search in the names of items
  location*: string = ""
    ## The base or trader word for info about money
  baseType*: string = ""
    ## The type of the current base
  typeIndex*: Natural = 0
    ## The index of the currently selected type of items
  eventIndex*: int = -1
    ## The index of the current event

proc refreshItemsList*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the list of items for trade
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index in playerShip.cargo.low .. playerShip.cargo.high:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index in baseCargo.low .. baseCargo.high:
      itemsIndexes.add(y = index)
  for i in itemsIndexes:
    if i == -1:
      break
    let
      protoIndex: Natural = playerShip.cargo[i].protoIndex
      itemType: string = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type.")
          return
    try:
      if typesList.find(item = itemType) == -1 and itemsList[
          protoIndex].price > 0:
        typesList.add(y = itemType)
    except:
      dialog = setError(message = "Can't add item type.")
      return
  let currentItemIndex: Positive = playerShip.cargo.len + 1
  for i in currentItemIndex..itemsIndexes.high:
    let
      protoIndex: Natural = baseCargo[itemsIndexes[i]].protoIndex
      itemType: string = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type3.")
          return
    try:
      if isBuyable(baseType = baseType, itemIndex = protoIndex,
          baseIndex = baseIndex) and baseCargo[itemsIndexes[i]].amount > 0 and
          typesList.find(item = itemType) == -1:
        typesList.add(y = itemType)
    except:
      dialog = setError(message = "Can't check if item is buyable.")
      return
  moneyAmount = moneyAmount(inventory = playerShip.cargo)
  moneyText = @[]
  moneyWidth = @[]
  if moneyAmount == 0:
    moneyText.add(y = "You don't have " & moneyName & " to buy anything")
  else:
    moneyText.add(y = "You have ")
    moneyText.add(y = $moneyAmount & " " & moneyName)
  if baseCargo[0].amount == 0:
    moneyText.add(y = " " & location & " doesn't have any " & moneyName & " to buy anything")
  else:
    moneyText.add(y = " " & location & " has ")
    moneyText.add(y = $baseCargo[0].amount & " " & moneyName)
  for text in moneyText:
    try:
      moneyWidth.add(y = text.getTextWidth)
    except:
      dialog = setError(message = "Can't get the width of the money text.")
      return
  setCargoText(baseIndex = baseIndex, dialog = dialog)

proc setTrade*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for trades UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  typesList = @["All"]
  baseCargo = @[]
  typeIndex = 0
  nameSearch = ""
  currentPage = 1
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  if baseIndex > 0:
    baseType = skyBases[baseIndex].baseType
    baseCargo = skyBases[baseIndex].cargo
    location = "Base"
  else:
    baseType = "0"
    baseCargo = traderCargo
    location = "Ship"
  refreshItemsList(dialog = dialog)
  if dialog == GameDialog.errorDialog:
    return

########################
# Setting the healing UI
########################

type
  BaseItemData* = object
    ## Used to store data about actions in bases, like healing, repair, recipes
    name*: string
      ## The text to display on the list
    cost*: Positive = 1
      ## The cost of action
    time*: Positive = 1
      ## The amount of time needed for the action
    id*: int
      ## The id of crew member, ship's module etc

var
  actionsList*: seq[BaseItemData] = @[]
    ## The list of actions for the selected items in a base

proc countCost(cost: var Natural) {.raises: [KeyError], tags: [RootEffect],
    contractual.} =
  ## Modify the selected price based on the game settings and a trader's skill
  ##
  ## * cost   - the cost which will be modified
  ##
  ## Returns modified parameter cost.
  cost = (cost.float * newGameSettings.pricesBonus).int
  if cost < 1:
    cost = 1
  countPrice(price = cost, traderIndex = findMember(order = talk))

proc setWoundedList*(dialog: var GameDialog): seq[BaseItemData] {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the list of wounded crew members
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened. Additionally it returns the list of wounded crew members.
  var cost, time: Natural = 0
  for index, member in playerShip.crew:
    if member.health == 100:
      continue
    try:
      healCost(cost = cost, time = time, memberIndex = index)
      countCost(cost = cost)
    except:
      dialog = setError(message = "Can't count heal cost.")
      return
    result.add(y = BaseItemData(name: member.name, cost: cost,
        time: time, id: index + 1))
  cost = 0
  time = 0
  try:
    healCost(cost = cost, time = time, memberIndex = -1)
    countCost(cost = cost)
  except:
    dialog = setError(message = "Can't count heal cost2.")
    return
  result.add(y = BaseItemData(name: "Heal all wounded crew members",
      cost: cost, time: time, id: 0))

proc setWounded*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for healing wounded crew members UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  setMoneyText(action = " to pay for healing", dialog = dialog)
  actionsList = setWoundedList(dialog = dialog)
  currentPage = 1

############################
# Setting the repair ship UI
############################

proc setRepairsList*(dialog: var GameDialog): seq[BaseItemData] {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the list of repair the player's ship's actions
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened. Additionally it returns the list of repairs actions.
  var cost, time: Natural = 0
  for index, module in playerShip.modules:
    if module.durability == module.maxDurability:
      continue
    try:
      repairCost(cost = cost, time = time, moduleIndex = index)
    except:
      dialog = setError(message = "Can't count repair cost.")
      return
    result.add(y = BaseItemData(name: module.name, cost: cost,
        time: time, id: index + 1))
  cost = 0
  time = 0
  try:
    repairCost(cost = cost, time = time, moduleIndex = -1)
    countCost(cost = cost)
  except:
    dialog = setError(message = "Can't count repair cost for slowl repairs.")
    return
  result.add(y = BaseItemData(name: "Slowly repair the whole ship",
      cost: cost, time: time, id: 0))
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    population: BasePopulation = getBasePopulation(baseIndex = baseIndex)
  if population > BasePopulation.small:
    cost = 0
    time = 0
    try:
      repairCost(cost = cost, time = time, moduleIndex = -2)
      countCost(cost = cost)
    except:
      dialog = setError(message = "Can't count repair cost for whole repair.")
      return
    result.add(y = BaseItemData(name: "Repair the whole ship",
        cost: cost, time: time, id: -1))
  if population > BasePopulation.medium:
    cost = 0
    time = 0
    try:
      repairCost(cost = cost, time = time, moduleIndex = -3)
      countCost(cost = cost)
    except:
      dialog = setError(message = "Can't count repair cost for quick repair.")
      return
    result.add(y = BaseItemData(name: "Quickly repair the whole ship",
        cost: cost, time: time, id: -2))

proc setRepairs*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for repair the player's ship's modules' UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  setMoneyText(action = " to pay for repairs", dialog = dialog)
  actionsList = setRepairsList(dialog = dialog)
  currentPage = 1

############################
# Setting the buy recipes UI
############################

proc setRecipesList*(dialog: var GameDialog): seq[BaseItemData] {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the list of available crafting recipes to buy in a base
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened. Additionally it returns the list of recipes to buy.
  var cost: Natural = 1
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  baseType = skyBases[baseIndex].baseType
  for index, recipe in recipesList:
    try:
      if index notin knownRecipes and index in basesTypesList[skyBases[
          baseIndex].baseType].recipes and recipe.reputation <= skyBases[
          baseIndex].reputation.level:
        cost = if getPrice(baseType = baseType, itemIndex = recipesList[
            index].resultIndex, quality = normal) > 0: getPrice(
            baseType = baseType, itemIndex = recipesList[index].resultIndex,
            quality = normal) * recipesList[index].difficulty * 10
          else:
            recipesList[index].difficulty * 10
        countCost(cost = cost)
        result.add(y = BaseItemData(name: itemsList[recipesList[
            index].resultIndex].name, cost: cost, time: 1, id: index.parseInt))
    except:
      dialog = setError(message = "Can't set the list of recipes to buy.")

proc setRecipes*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for buying crafting recipes UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  setMoneyText(action = " to buy anything", dialog = dialog)
  actionsList = setRecipesList(dialog = dialog)
  currentPage = 1

#########################
# Setting the shipyard UI
#########################

type ModulesSortOrders* = enum
  ## Sorting order of the list of modules to install or remove
  none, nameAsc, nameDesc, typeAsc, typeDesc, sizeAsc, sizeDesc, materialAsc,
    materialDesc, priceAsc, priceDesc

const defaultModulesSortOrder*: ModulesSortOrders = none
  ## Default sorting order for list of modules to install or remove

var
  modulesText*: array[5, string] = ["You have used", "0",
      "modules space from max", "0", "allowed."]
    ## The text with information about money in player's ship's cargo and trader
  modulesWidth*: array[5, cfloat] = [0, 0, 0, 0, 0]
    ## The width in pixels of the text with information about money
  modulesAmount*: tuple[installed, max: Natural] = (installed: 0, max: 0)
    ## The information about modules installed, space on the player's ship
  modulesSortOrder*: ModulesSortOrders = defaultModulesSortOrder
    ## The current sorting order of the list of modules in a shipyard
  modulesIndexes*: seq[int] = @[]
    ## The list of indexes of modules in a shipyard
  currentTab*: cint = 0
    ## The current tab on the list of modules in shipyard
  maxModuleSize*: Positive = 1
    ## The maximum size of a module which is allowed to install on the player's
    ## ship

proc setModulesList*(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the list of available modules to install or remove in a shipyard
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  modulesIndexes = @[]
  if currentTab == 1:
    for index in playerShip.modules.low .. playerShip.modules.high:
      modulesIndexes.add(y = index)
  else:
    for index, module in modulesList:
      try:
        if typeIndex > 0 and typeIndex != modulesList[index].mType.ord:
          continue
      except:
        dialog = setError(message = "Can't check a module's type.")
      try:
        if module.price == 0 or skyBases[baseIndex].reputation.level <
            module.reputation:
          continue
      except:
        dialog = setError(message = "Can't get proto module price.")
      modulesIndexes.add(y = index)

proc setShipyard*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for shipyard UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  typesList = @["Any", "Engines", "Cabins", "Cockpits", "Turrets", "Guns",
      "Cargo bays", "Hulls", "Armors", "Battering rams", "Alchemy labs",
      "Furnaces", "Water collectors", "Workshops", "Greenhouses",
      "Medical rooms", "Harpoon guns", "Training rooms"]
  typeIndex = 0
  nameSearch = ""
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  setMoneyText(action = " to install anything", dialog = dialog)
  currentPage = 1
  for module in playerShip.modules:
    if module.mType == ModuleType2.hull:
      modulesText[1] = $module.installedModules
      modulesAmount.installed = module.installedModules
      modulesText[3] = $module.maxModules
      modulesAmount.max = module.maxModules
      maxModuleSize = try:
          modulesList[module.protoIndex].value
        except:
          dialog = setError(message = "Can't get max size.")
          0
      break
  for index, text in modulesText:
    try:
      modulesWidth[index] = text.getTextWidth
    except:
      dialog = setError(message = "Can't get the width of the money text.")
  setModulesList(dialog = dialog)

###################################
# Setting the available missions UI
###################################

var
  missionsText*: array[3, string] = ["You can take", "0", "more missions from the base."]
    ## The text with information about how many missions can be taken
  missionsWidth*: array[3, cfloat] = [0, 0, 0]
    ## The width in pixels of the text with information about missions
  missionsIndexes*: seq[Natural] = @[]
    ## The list of indexes of available missions in a base

proc setMissions*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for missions available in the base UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var missionsLimit: Natural = (case skyBases[baseIndex].reputation.level
    of 0..25:
      1
    of 26..50:
      3
    of 51..75:
      5
    of 76..100:
      10
    else:
      0)
  for mission in acceptedMissions:
    if mission.startBase == baseIndex:
      missionsLimit.dec
      if missionsLimit == 0:
        break
  missionsText[1] = $missionsLimit
  for index, text in missionsText:
    try:
      missionsWidth[index] = text.getTextWidth
    except:
      dialog = setError(message = "Can't get the width of the missions text.")
  missionsIndexes = @[]
  for index, _ in skyBases[baseIndex].missions:
    missionsIndexes.add(y = index)
  currentPage = 1
  mapPreview = false

#####################
# Setting the loot UI
#####################

proc refreshLootList*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the list of items for loot
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index in playerShip.cargo.low..playerShip.cargo.high:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index in baseCargo.low..baseCargo.high:
      itemsIndexes.add(y = index)
  for i in itemsIndexes:
    if i == -1:
      break
    let
      protoIndex: Natural = playerShip.cargo[i].protoIndex
      itemType: string = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type.")
          return
    if typesList.find(item = itemType) == -1:
      typesList.add(y = itemType)
  let currentItemIndex: Positive = playerShip.cargo.len + 1
  for i in currentItemIndex..itemsIndexes.high:
    let
      protoIndex: Natural = baseCargo[itemsIndexes[i]].protoIndex
      itemType: string = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type3.")
          return
    if typesList.find(item = itemType) == -1:
      typesList.add(y = itemType)
  moneyAmount = moneyAmount(inventory = playerShip.cargo)
  var freeSpace: int = try:
      freeCargo(amount = 0)
    except:
      dialog = setError(message = "Can't get free space.")
      return
  if freeSpace < 0:
    freeSpace = 0
  cargoText[1] = $freeSpace & " kg"
  cargoText[3] = $countFreeCargo(baseIndex = baseIndex) & " free space"
  for index, text in cargoText:
    try:
      cargoWidth[index] = text.getTextWidth
    except:
      dialog = setError(message = "Can't get the width of the money text.")
      return

proc setLoot*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for loot UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  currentPage = 1
  typesList = @["All"]
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  baseCargo = skyBases[baseIndex].cargo
  baseType = skyBases[baseIndex].baseType
  location = "Base"
  refreshLootList(dialog = dialog)
  if dialog == GameDialog.errorDialog:
    return

##########################
# Setting the ship info UI
##########################

type
  CrewData* = object
    ## Stores data needed to show information about the player's ship's crew
    ## members
    index*: int
      ## The index of the crew member
    checked*: bool
      ## If true, the crew member is checked, otherwise false

var
  needClean*: bool = false
    ## If true, the ship needs cleaning
  needRepair*: bool = false
    ## If true, the ship needs repairs
  crewSkillsList*: seq[string] = @["Highest"]
    ## The list of skills to which the player's ship's crew members can be
    ## listed
  crewDataList*: seq[CrewData] = @[]
    ## The list of data related to the player's ship's crew members

proc refreshCrewList*() {.raises: [], tags: [], contractual.} =
  ## Set the list of crew members in the player's ship
  crewDataList = @[]
  crewList = @[]
  for index, member in playerShip.crew:
    crewList.add(y = member.name)
    crewDataList.add(y = CrewData(index: index, checked: false))

proc refreshCargoList*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the list of items in the player's ship's cargo
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Set cargo info text
  setCargoText(baseIndex = 0, dialog = dialog)
  typesList = @["All"]
  # Set indexes of items in the cargo
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index in playerShip.cargo.low .. playerShip.cargo.high:
      itemsIndexes.add(y = index)
  # Set the list of types of items in the cargo
  for i in itemsIndexes:
    let
      protoIndex: Natural = playerShip.cargo[i].protoIndex
      itemType: string = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type.")
          return
    try:
      if typesList.find(item = itemType) == -1 and itemsList[
          protoIndex].price > 0:
        typesList.add(y = itemType)
    except:
      dialog = setError(message = "Can't add item type.")
      return

proc setShipInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for the player's ship's info screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  for module in playerShip.modules:
    if module.durability < module.maxDurability:
      needRepair = true
    if module.durability > 0 and module.mType == ModuleType2.cabin and
        module.cleanliness < module.quality:
      needClean = true
    if needRepair and needClean:
      break
  if crewSkillsList.len == 1:
    for skill in skillsList.values:
      crewSkillsList.add(y = skill.name)
  refreshCrewList()
  modulesIndexes = @[]
  for index in playerShip.modules.low..playerShip.modules.high:
    modulesIndexes.add(y = index)
  refreshCargoList(dialog = dialog)
  crewIndex = 0

#########################
# Setting the crafting UI
#########################

type
  RecipeType* = enum
    ## Types of recipes to craft
    craftType = "Craft",
    study = "Study",
    deconstruct = "Deconstruct"
  RecipeData* = object
    ## Stores data needed to show information about an available recipe
    index*: string
      ## The index of the recipe
    name*: string
      ## The name of the recipe
    craftable*: bool
      ## If true, the recipe is craftable
    workplace*: bool
      ## If true, there is a workshop to craft the recipe
    tools*: bool
      ## If true, there are tools to craft the recipe
    materials*: bool
      ## If true, there are materials to craft the recipe
    workshop*: ModuleType
      ## The module type in which the recipe is crafted
    recipeType*: RecipeType
      ## The type of the recipe
  WorkshopData* = object
    ## Stores data needed to show information about an installed workshop
    index*: Natural
      ## The index of the workshop in the player's ship
    name*: string
      ## The name of the workshop
    order*: string
      ## Current order of the workshop
    workers*: string
      ## The list of current workers in the workshop
    tooltip*: string
      ## The tooltip to show to the player

var
  workshopsList*: seq[string] = @[]
    ## The list of names of workshops installed on the player's ship
  workshopsList2*: seq[WorkshopData] = @[]
    ## The list of workshops installed on the player's ship
  workshopIndex*: Natural = 0
    ## The index of the currently selected workshop
  workshopType*: Natural = 0
    ## The index of the currently selected workshops' type
  studies*: seq[Positive] = @[]
    ## The list of available recipes to study
  deconstructs*: seq[Positive] = @[]
    ## The list of available recipes to deconstruct
  availableRecipes*: seq[RecipeData] = @[]
    ## The list of available recipes

proc setWorkshopsList*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the list of workshops in the crafting info screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  workshopsList = @["All"]
  workshopsList2 = @[]
  for index, module in playerShip.modules:
    if module.mType == workshop:
      workshopsList.add(y = module.name)
      var
        recipeName2: string = try:
            getWorkshopRecipeName(workshop = index)
        except:
          dialog = setError(message = "Can't get the recipe name.")
          return
        tooltipText: string = "Cancel the selected order"
      if recipeName2.len == 0:
        recipeName2 = "Not set"
        tooltipText = "Set a new order for the workshop"
      var workers: string = ""
      var haveWorkers: bool = false
      for worker in module.owner:
        if worker > -1:
          if haveWorkers:
            workers.add(y = ", ")
          haveWorkers = true
          workers.add(y = playerShip.crew[worker].name)
      if not haveWorkers:
        workers = "none"
      workshopsList2.add(y = WorkshopData(index: index, name: module.name,
          order: recipeName2, workers: workers, tooltip: tooltipText))

proc setCrafting*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for the crafting info screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  nameSearch = ""
  workshopIndex = 0
  workshopType = 0
  typeIndex = 0
  studies = @[]
  deconstructs = @[]
  setWorkshopsList(dialog = dialog)
  for item in playerShip.cargo:
    for recipeIndex, recipe in recipesList:
      if recipe.resultIndex == item.protoIndex:
        if recipeIndex notin knownRecipes and item.protoIndex notin studies:
          studies.add(y = item.protoIndex)
        if recipe.materialAmounts[0] > 1 and recipe.resultAmount == 1:
          deconstructs.add(y = item.protoIndex)
  if availableRecipes.len != knownRecipes.len + studies.len + deconstructs.len:
    availableRecipes = @[]
    for recipe in knownRecipes:
      try:
        var rec: RecipeData = RecipeData(index: recipe, name: "Craft " &
            itemsList[recipesList[recipe].resultIndex].name,
            workshop: recipesList[recipe].workplace, recipeType: craftType)
        isCraftable(recipe = recipesList[recipe], canCraft = rec.craftable,
            hasWorkplace = rec.workplace, hasTool = rec.tools,
            hasMaterials = rec.materials)
        availableRecipes.add(y = rec)
      except:
        dialog = setError(message = "Can't add a crafting recipe.")
        return
    var canCraft, hasTool, hasWorkplace: bool = false
    try:
      checkStudyPrerequisities(canCraft = canCraft, hasTool = hasTool,
          hasWorkplace = hasWorkplace)
    except:
      dialog = setError(message = "Can't check study prerequisities.")
      return
    for recipe in studies:
      try:
        var rec: RecipeData = RecipeData(index: "Study " & $recipe,
            name: "Study " & itemsList[recipe].name, craftable: canCraft,
            workplace: hasWorkplace, tools: hasTool, materials: true,
            recipeType: study, workshop: alchemyLab)
        availableRecipes.add(y = rec)
      except:
        dialog = setError(message = "Can't add a study recipe.")
        return
    for recipe in deconstructs:
      try:
        var rec: RecipeData = RecipeData(index: "Deconstruct " & $recipe,
            name: "Deconstruct " & itemsList[recipe].name, craftable: canCraft,
            workplace: hasWorkplace, tools: hasTool, materials: true,
            recipeType: deconstruct, workshop: alchemyLab)
        availableRecipes.add(y = rec)
      except:
        dialog = setError(message = "Can't add a study recipe.")
        return

##########################
# Setting the knowledge UI
##########################

type
  DataType* = enum
    ## Types of knowledge data
    event, mission
  BaseData* = object
    ## Stores data needed to show information about a base
    index*: Natural
      ## The index of the base
    name*: string
      ## The name of the base
    distance*: Natural
      ## The distance to the base from the current player's position
    coords*: string
      ## The coordinates of the base
    population*: BasePopulation
      ## The population of the base
    size*: BasesSize
      ## The side of the base
    owner*: string
      ## The owner of the base
    baseType*: string
      ## The type of the base
    reputation*: string
      ## The player's reputaion in the base
    visited*: bool
      ## If true, the base was visited by the player
  KnowledgeData* = object
    ## Stores data needed to show information about an event or an mission
    index*: Natural
      ## The index of the event or the mission
    name*: string
      ## The name of the event or the mission
    distance*: Natural
      ## The distance to the event or the mission from the current player's
      ## position
    coords*: string
      ## The coordinates of the event or the mission
    color*: ColorsNames
      ## The color used show the event's or the mission's info
    case dataType*: DataType
    of event:
      details*: string
        ## Additional information about the event
    of mission:
      timeLimit*: string
        ## Limit of time for the mission
      baseReward*: string
        ## The base amount of money for the mission

const
  basesStatuses*: array[3, string] = ["Any", "Not visited", "Visited"]
    ## The list of bases statuses to show

var
  basesTList*: seq[string] = @[]
    ## The list of names of types of all bases in the game
  basesOwners*: seq[string] = @[]
    ## The list of owners of all bases in the game
  knownBasesList*: seq[BaseData] = @[]
    ## The list of known bases
  knownEventsList*: seq[KnowledgeData] = @[]
    ## The list of known events
  missionsUIList*: seq[KnowledgeData] = @[]
    ## The list of accepted missions
  knownStoriesList*: seq[string] = @[]
    ## The list of known stories

proc setKnowledge*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for the knowledge info screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  nameSearch = ""
  basesTList = @["Any"]
  for baseType in basesTypesList.values:
    basesTList.add(y = baseType.name)
  basesOwners = @["Any"]
  for faction in factionsList.values:
    basesOwners.add(y = faction.name)
  # Set the list of known bases
  knownBasesList = @[]
  for index, base in skyBases:
    if base.known:
      try:
        knownBasesList.add(y = BaseData(index: index, name: base.name,
            distance: countDistance(destinationX = base.skyX,
            destinationY = base.skyY), coords: "X: " & $base.skyX & " Y: " &
            $base.skyY, population: getBasePopulation(baseIndex = index),
            size: base.size, owner: factionsList[base.owner].name,
            baseType: basesTypesList[base.baseType].name,
            reputation: getReputationText(
            reputationLevel = base.reputation.level),
            visited: base.visited.year > 0))
      except:
        dialog = setError(message = "Can't get a base info")
  # Set the list of known events
  knownEventsList = @[]
  for index, event in eventsList:
    try:
      var eventData: KnowledgeData = KnowledgeData(index: index, name: "",
          distance: countDistance(destinationX = event.skyX,
          destinationY = event.skyY), coords: "X: " & $event.skyX & " Y: " &
          $event.skyY, dataType: DataType.event)
      case event.eType
      of enemyShip:
        eventData.name = "Enemy ship spotted"
        eventData.details = protoShipsList[event.shipIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: redColor)
      of fullDocks:
        eventData.name = "Full docks in base"
        eventData.details = skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: cyanColor)
      of attackOnBase:
        eventData.name = "Base is under attack"
        eventData.details = skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: redColor)
      of disease:
        eventData.name = "Disease in base"
        eventData.details = skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: goldenColor)
      of enemyPatrol:
        eventData.name = "Enemy patrol"
        eventData.details = skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: redColor)
      of doublePrice:
        eventData.name = "Double price in base"
        eventData.details = itemsList[event.itemIndex].name & " in " & skyBases[
            skyMap[event.skyX][event.skyY].baseIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: limeColor)
      of trader:
        eventData.name = "Friendly trader spotted"
        eventData.details = protoShipsList[event.shipIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: greenColor)
      of friendlyShip:
        eventData.name = "Friendly ship spotted"
        eventData.details = protoShipsList[event.shipIndex].name
        eventData.color = (if playerShip.skyX == event.skyX and
            playerShip.skyY == event.skyY: yellowColor else: greenColor)
      of EventsTypes.none, baseRecovery:
        discard
      knownEventsList.add(y = eventData)
    except KeyError:
      dialog = setError(message = "Can't set an event info")
  # Set the list of accepted missions
  missionsUIList = @[]
  for index, mission in acceptedMissions:
    try:
      var missionTime: string = ""
      minutesToDate(minutes = mission.time, infoText = missionTime)
      var missionData: KnowledgeData = KnowledgeData(index: index, name: "",
          distance: countDistance(destinationX = mission.targetX,
          destinationY = mission.targetY), coords: "X: " & $mission.targetX &
          " Y: " & $mission.targetY, baseReward: $((mission.reward.float *
          mission.multiplier).Natural) & " " & moneyName, color: (
          if playerShip.skyX == mission.targetX and playerShip.skyY ==
          mission.targetY: yellowColor else: tableTextColor),
          dataType: DataType.mission, timeLimit: missionTime)
      case mission.mType
      of deliver:
        missionData.name = "Deliver " & itemsList[mission.itemIndex].name &
            " to " & skyBases[skyMap[mission.targetX][
            mission.targetY].baseIndex].name
      of patrol:
        missionData.name = "Patrol selected area"
      of explore:
        missionData.name = "Explore selected area"
      of destroy:
        missionData.name = "Destroy " & protoShipsList[mission.shipIndex].name
      of passenger:
        missionData.name = "Passenger to " & skyBases[skyMap[mission.targetX][
            mission.targetY].baseIndex].name
      missionsUIList.add(y = missionData)
    except KeyError:
      dialog = setError(message = "Can't set a mission info")
  # Set the list of known stories
  knownStoriesList = @[]
  for index, story in finishedStories:
    try:
      knownStoriesList.add(y = storiesList[story.index].name)
    except KeyError:
      dialog = setError(message = "Can't set the known story")

###########################
# Setting the statistics UI
###########################

type
  StatItemData* = object
    ## Stores data needed to show information about a statistic's item
    name*: string
      ## The name of the item
    amount*: Natural
      ## The amount of the item

var
  statisticsValues*: array[10, string] = ["", "", "", "", "", "", "", "", "", ""]
    ## Values of the game's statistics
  finishedCrafts*: seq[StatItemData] = @[]
    ## The list of finished crafting orders
  finishedMissions*: seq[StatItemData] = @[]
    ## The list of finished missions
  finishedGoals*: seq[StatItemData] = @[]
    ## The list of finished goals
  destroyedShips*: seq[StatItemData] = @[]
    ## The list of destroyed ships
  killedMobs*: seq[StatItemData] = @[]
    ## The list of killed mobs


proc setStatistics*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for the game's statistics screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  statisticsValues[0] = $getGamePoints()
  let minutesDiff: int = (gameDate.minutes + (gameDate.hour * 60) + (
      gameDate.day * 1_440) + (gameDate.month * 43_200) + (gameDate.year *
      518_400)) - 829_571_520
  minutesToDate(minutes = minutesDiff, infoText = statisticsValues[1])
  var visitedPercent: float = (gameStats.basesVisited.float / 1_024.0) * 100.0
  statisticsValues[2] = try:
      $gameStats.basesVisited & " (" & fmt"{visitedPercent:5.3f}" & "%)"
    except:
      dialog = setError(message = "Can't show info about visited bases.")
      return
  visitedPercent = (gameStats.mapVisited.float / (1_024.0 * 1_024.0)) * 100.0
  if visitedPercent < 0.001:
    visitedPercent = 0.001
  statisticsValues[3] = try:
      fmt"{visitedPercent:5.3f}" & "%"
    except:
      dialog = setError(message = "Can't show info about discovered map.")
      return
  statisticsValues[4] = $gameStats.distanceTraveled
  var
    totalFinished: Natural = 0
    statsList: seq[StatisticsData] = gameStats.craftingOrders
  for craftingOrder in statsList:
    totalFinished += craftingOrder.amount
  statisticsValues[5] = $totalFinished
  finishedCrafts = @[]
  if totalFinished > 0:
    for stat in statsList:
      try:
        finishedCrafts.add(y = StatItemData(name: itemsList[recipesList[
            stat.index].resultIndex].name, amount: stat.amount))
      except:
        dialog = setError(message = "Can't show finished crafting orders.")
        return
  totalFinished = 0
  statsList = gameStats.finishedMissions
  for finishedMission in statsList:
    totalFinished += finishedMission.amount
  var missionsPercent: int = 0
  if gameStats.acceptedMissions > 0:
    missionsPercent = ((totalFinished.float /
        gameStats.acceptedMissions.float) * 100.0).int
  statisticsValues[6] = $totalFinished & " (" & $missionsPercent & "%)"
  finishedMissions = @[]
  if totalFinished > 0:
    for stat in statsList:
      try:
        case parseEnum[MissionsTypes](s = stat.index)
        of deliver:
          finishedMissions.add(y = StatItemData(name: "Delivered items",
              amount: stat.amount))
        of patrol:
          finishedMissions.add(y = StatItemData(name: "Patroled areas",
              amount: stat.amount))
        of destroy:
          finishedMissions.add(y = StatItemData(name: "Destroyed ships",
              amount: stat.amount))
        of explore:
          finishedMissions.add(y = StatItemData(name: "Explored areas",
              amount: stat.amount))
        of passenger:
          finishedMissions.add(y = StatItemData(name: "Passengers transported",
              amount: stat.amount))
      except:
        dialog = setError(message = "Can't show finished missions.")
        return
  totalFinished = 0
  statsList = gameStats.finishedGoals
  for finishedGoal in statsList:
    totalFinished += finishedGoal.amount
  statisticsValues[7] = $totalFinished
  finishedGoals = @[]
  if totalFinished > 0:
    for stat in statsList:
      try:
        finishedGoals.add(y = StatItemData(name: goalText(
            index = stat.index.parseInt), amount: stat.amount))
      except:
        dialog = setError(message = "Can't show finished goals.")
        return
  totalFinished = 0
  statsList = gameStats.destroyedShips
  for destroyedShip in statsList:
    totalFinished += destroyedShip.amount
  statisticsValues[8] = $totalFinished
  destroyedShips = @[]
  if totalFinished > 0:
    for stat in statsList:
      try:
        destroyedShips.add(y = StatItemData(name: protoShipsList[
            stat.index.parseInt].name, amount: stat.amount))
      except:
        dialog = setError(message = "Can't show destroyed ships.")
        return
  totalFinished = 0
  statsList = gameStats.killedMobs
  for killedMob in statsList:
    totalFinished += killedMob.amount
  statisticsValues[9] = $totalFinished
  killedMobs = @[]
  if totalFinished > 0:
    for stat in statsList:
      try:
        killedMobs.add(y = StatItemData(name: stat.index, amount: stat.amount))
      except:
        dialog = setError(message = "Can't show killed mobs.")
        return

#####################
# Setting the help UI
#####################

type
  TextTags* = enum
    ## Help text tags
    none, underline, bold, italic, special
  HelpUIText* = object
    ## Used to store data about help text to display
    text*: string
    tag*: TextTags
    width*: cfloat
  TextsSeq = seq[HelpUIText]

var
  selectedHelp*: Natural = 0
    ## The index of the selected help topic
  helpContent*: seq[TextsSeq] = @[]
    ## The content of the selected help topic

proc setHelpContent*(content: string; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the content of the selected help topic to show
  ##
  ## * content - the content of the selected help topic
  ## * dialog  - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  type
    VariablesData = object
      name, value: string
    FontTag = object
      tag: string
      textTag: TextTags
  let
    variables: array[1..11, VariablesData] = try:
        [VariablesData(name: "MoneyName", value: moneyName), VariablesData(
            name: "FuelName", value: itemsList[findProtoItem(
            itemType = fuelType)].name), VariablesData(name: "StrengthName",
            value: attributesList[strengthIndex].name), VariablesData(
            name: "PilotingSkill", value: skillsList[pilotingSkill].name),
            VariablesData(name: "EngineeringSkill", value: skillsList[
            engineeringSkill].name), VariablesData(name: "GunnerySkill",
            value: skillsList[gunnerySkill].name), VariablesData(
            name: "TalkingSkill", value: skillsList[talkingSkill].name),
            VariablesData(name: "PerceptionSkill", value: skillsList[
            perceptionSkill].name), VariablesData(name: "ConditionName",
            value: attributesList[conditionIndex].name), VariablesData(
            name: "DodgeSkill", value: skillsList[dodgeSkill].name),
            VariablesData(name: "UnarmedSkill", value: skillsList[
            unarmedSkill].name)]
      except:
        dialog = setError(message = "Can't set help variables")
        return
    accelNames: array[1..25, string] = [mapAccelerators[5], mapAccelerators[
        6], mapAccelerators[7], mapAccelerators[8], mapAccelerators[9],
        mapAccelerators[10], mapAccelerators[11], mapAccelerators[12],
        mapAccelerators[13], mapAccelerators[14], menuAccelerators[1],
        menuAccelerators[2], menuAccelerators[3], menuAccelerators[4],
        menuAccelerators[5], menuAccelerators[6], mapAccelerators[2],
        menuAccelerators[7], menuAccelerators[9], menuAccelerators[10],
        menuAccelerators[11], mapAccelerators[1], menuAccelerators[8],
        mapAccelerators[3], mapAccelerators[4]]
  const
    fontTags: array[1..3, FontTag] = [FontTag(tag: "b", textTag: bold),
        FontTag(tag: "u", textTag: underline), FontTag(tag: "i",
        textTag: italic)]
    flagsTags: array[1..8, string] = ["diseaseimmune", "nofatigue",
        "nomorale", "naturalarmor", "toxicattack", "sentientships",
        "fanaticism", "loner"]
    basesFlags: array[1..4, string] = ["shipyard", "temple", "blackmarket", "barracks"]
  var
    oldIndex: int = 0
    texts: TextsSeq = @[]
  nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpFont],
      fontSize = gameSettings.helpFontSize + 10)

  proc setFont(tag: TextTags) {.raises: [], tags: [], contractual.} =
    ## Set the font for help for count the length of a text
    ##
    ## * tag - the text formatting tag, like bold, italic, etc
    if tag == bold:
      nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpBoldFont],
          fontSize = gameSettings.helpFontSize + 10)
    elif tag == italic:
      nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpItalicFont],
          fontSize = gameSettings.helpFontSize + 10)
    else:
      nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpFont],
          fontSize = gameSettings.helpFontSize + 10)

  while true:
    var
      startIndex: int = content.find(sub = '{', start = oldIndex)
      uiText: HelpUIText = HelpUIText()
    while content[oldIndex] == '\n':
      uiText.text = "\n"
      uiText.width = 0
      texts.add(y = uiText)
      oldIndex.inc
    if startIndex == -1:
      uiText.text = content[oldIndex..^1]
      uiText.width = try:
          uiText.text.getTextWidth
        except:
          dialog = setError(message = "Can't get the text's width")
          return
      texts.add(y = uiText)
      break
    uiText.text = content[oldIndex..startIndex - 1]
    uiText.width = try:
        uiText.text.getTextWidth
      except:
        dialog = setError(message = "Can't get the text's width 2")
        return
    texts.add(y = uiText)
    var endIndex: int = content.find(sub = '}', start = startIndex) - 1
    let tagText: string = content[startIndex + 1..endIndex]
    nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpBoldFont],
        fontSize = gameSettings.helpFontSize + 10)
    for variable in variables:
      if tagText == variable.name:
        uiText = try:
            HelpUIText(text: variable.value, tag: special,
                width: variable.value.getTextWidth)
          except:
            dialog = setError(message = "Can't set a variable")
            return
        texts.add(y = uiText)
        break
    for index, accel in accelNames:
      if tagText == "GameKey " & $index:
        uiText = try:
            HelpUIText(text: accel, tag: special, width: accel.getTextWidth)
          except:
            dialog = setError(message = "Can't set an accelerator")
            return
        texts.add(y = uiText)
        break
    for tag in fontTags:
      if tagText == tag.tag:
        setFont(tag = tag.textTag)
        startIndex = content.find(sub = '{', start = endIndex) - 1
        uiText = try:
            HelpUIText(text: content[endIndex + 2..startIndex],
                tag: tag.textTag, width: content[endIndex +
                2..startIndex].getTextWidth)
          except:
            dialog = setError(message = "Can't set a tag")
            return
        texts.add(y = uiText)
        endIndex = content.find(sub = '}', start = startIndex) - 1
        break
    nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpFont],
        fontSize = gameSettings.helpFontSize + 10)
    for tag in flagsTags:
      if tagText == tag:
        var factionsWithFlag: string = ""
        for faction in factionsList.values:
          if tagText in faction.flags:
            if factionsWithFlag.len > 0:
              factionsWithFlag.add(y = ", ")
            factionsWithFlag.add(y = faction.name)
        factionsWithFlag.removeSuffix(suffix = ", ")
        uiText = try:
            HelpUIText(text: factionsWithFlag, tag: none,
                width: factionsWithFlag.getTextWidth)
          except:
            dialog = setError(message = "Can't set the factions")
            return
        texts.add(y = uiText)
        break
    for tag in basesFlags:
      if tagText != tag:
        continue
      var basesWithFlag: string = ""
      for baseType in basesTypesList.values:
        if tagText in baseType.flags:
          if basesWithFlag.len > 0:
            basesWithFlag.add(y = ", ")
          basesWithFlag.add(y = baseType.name)
      basesWithFlag.removeSuffix(suffix = ", ")
      uiText = try:
          HelpUIText(text: basesWithFlag, tag: none,
              width: basesWithFlag.getTextWidth)
        except:
          dialog = setError(message = "Can't set the bases")
          return
      texts.add(y = uiText)
      break
    oldIndex = endIndex + 2
  helpContent = @[]
  var
    width: float = windowWidth - 35
    row: TextsSeq = @[]
    index: Natural = 0
  while index < texts.high:
    if texts[index].text.len == 0:
      texts.delete(i = index)
      continue
    if texts[index].text == "\n":
      index.inc
      continue
    var splitText: seq[string] = texts[index].text.splitLines(keepEol = true)
    if splitText.len > 1:
      try:
        let addNewLine: bool = splitText[0].endsWith(suffix = "\n")
        splitText[0].stripLineEnd
        setFont(tag = texts[index].tag)
        texts[index] = HelpUIText(text: splitText[0], tag: texts[index].tag,
            width: splitText[0].getTextWidth)
        if addNewLine:
          index.inc
          texts.insert(item = HelpUIText(text: "\n", tag: none, width: 0),
              i = index)
      except:
        dialog = setError(message = "Can't split text")
        return
      var newIndex: Positive = index + 1
      nuklearSetDefaultFont(defaultFont = fonts[FontsNames.helpFont],
          fontSize = gameSettings.helpFontSize + 10)
      for i in 1..splitText.high:
        if splitText[i] == "\n":
          texts.insert(item = HelpUIText(text: "\n", tag: none, width: 0),
              i = newIndex)
          newIndex.inc
          continue
        let addNewLine: bool = splitText[i].endsWith(suffix = "\n")
        splitText[i].stripLineEnd
        if splitText[i].len > 0:
          try:
            texts.insert(item = HelpUIText(text: splitText[i], tag: none,
                width: splitText[i].getTextWidth), i = newIndex)
            newIndex.inc
            if addNewLine:
              texts.insert(item = HelpUIText(text: "\n", tag: none, width: 0),
                  i = newIndex)
              newIndex.inc
          except:
            dialog = setError(message = "Can't insert text")
            return
    index.inc
  index = 0
  while index < texts.len:
    if texts[index].text == "\n":
      if row.len == 0:
        row.add(y = HelpUIText())
      helpContent.add(y = row)
      row = @[]
      width = windowWidth - 35
      index.inc
      continue
    if texts[index].width <= width:
      row.add(y = texts[index])
      width -= (texts[index].width + 3)
      index.inc
      if width < 10 or index == texts.len:
        width = 0
    else:
      var endIndex: Positive = ((width / texts[index].width) * texts[
          index].text.len.float).Positive
      if endIndex >= texts[index].text.len:
        endIndex = texts[index].text.len - 1
      setFont(tag = texts[index].tag)
      var textWidth: float = try:
            texts[index].text[0..endIndex].getTextWidth
          except:
            dialog = setError(message = "Can't count text width")
            return
      while textWidth > width and endIndex > 1:
        endIndex.dec
        try:
          textWidth = texts[index].text[0..endIndex].getTextWidth
        except:
          dialog = setError(message = "Can't count text width 2")
          return
      var newText: HelpUIText = HelpUIText(text: texts[index].text[0..endIndex],
          tag: texts[index].tag, width: textWidth)
      row.add(y = newText)
      width = 0
      endIndex.inc
      newText = try:
          HelpUIText(text: texts[index].text[endIndex..^1], tag: texts[
              index].tag, width: texts[index].text[endIndex..^1].getTextWidth)
        except:
          dialog = setError(message = "Can't get new text")
          return
      texts[index] = newText
    if width == 0:
      helpContent.add(y = row)
      row = @[]
      width = windowWidth - 35
  nuklearSetDefaultFont(defaultFont = fonts[UIFont],
      fontSize = gameSettings.interfaceFontSize + 10)

proc setHelp*(dialog: var GameDialog; helpIndex: Natural = 0) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the data for the in-game help screen
  ##
  ## * dialog    - the current in-game dialog displayed on the screen
  ## * helpIndex - the index of help topic to show. Can be empty
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  var
    index: Natural = 0
    content: string = ""
  for title, entry in helpList:
    if index == helpIndex:
      selectedHelp = index
      content = entry.text
      break
    index.inc
  setHelpContent(content = content, dialog = dialog)
  dialog = none

########################
# Setting the options UI
########################

var generalOptions*: array[17, Natural] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0]

proc setOptions*() {.raises: [], tags: [], contractual.} =
  ## Set the data for the game options screen
  currentTab = 0
  generalOptions[0] = gameSettings.autoRest.ord
  generalOptions[1] = gameSettings.undockSpeed.ord
  generalOptions[2] = gameSettings.autoCenter.ord
  generalOptions[3] = gameSettings.autoReturn.ord
  generalOptions[4] = gameSettings.autoDestination.ord
  generalOptions[5] = gameSettings.autoFinish.ord
  generalOptions[6] = gameSettings.autoAskForBases.ord
  generalOptions[7] = gameSettings.autoAskForEvents.ord
  generalOptions[8] = gameSettings.lowFuel
  generalOptions[9] = gameSettings.lowDrinks
  generalOptions[10] = gameSettings.lowFood
  generalOptions[11] = gameSettings.autoMoveStop.ord
  generalOptions[12] = gameSettings.messagesLimit
  generalOptions[13] = gameSettings.savedMessages
  generalOptions[14] = gameSettings.messagesOrder.ord
  generalOptions[15] = gameSettings.autoSave.ord
  generalOptions[16] = gameSettings.waitMinutes
