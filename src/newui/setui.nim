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

## Provides code related to training skills in bases, like show the UI,
## set the skill to train, etc.

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basescargo, basesship, basestrade, basestypes, combat, config,
    crew, game, items, maps, missions, shipscargo, shipscrew, types]
import coreui, errordialog, utilsui2

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
  var freeSpace = try:
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
    var skillLevel = 0
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
      let combatStarted = startCombat(enemyIndex = eventsList[skyMap[
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
      protoIndex = playerShip.cargo[i].protoIndex
      itemType = try:
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
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = try:
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
  var missionsLimit = (case skyBases[baseIndex].reputation.level
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
      protoIndex = playerShip.cargo[i].protoIndex
      itemType = try:
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
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = try:
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
  var freeSpace = try:
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
  for index in playerShip.crew.low..playerShip.crew.high:
    crewDataList.add(y = CrewData(index: index, checked: false))

proc setShipInfo*() {.raises: [], tags: [], contractual.} =
  ## Set the data for the player's ship's info screen
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
