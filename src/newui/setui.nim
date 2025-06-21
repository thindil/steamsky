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
import ../[basestrade, basestypes, combat, crew, crewinventory, game, maps, shipscargo, shipscrew, types]
import coreui, errordialog, utilsui2

#######################
# Setting the school UI
#######################

var
  moneyIndex2*: int = -1
    ## Index of money in the player's ship's cargo
  moneyText*: seq[string] = @[]
    ## The text with information about money in player's ship's cargo and trader
  crewList*: seq[string] = @[]
    ## The list of names of the player's ship's crew members
  schoolSkillsList*: seq[string] = @[]
    ## The list of skills which the crew member can learn, with prices
  moneyWidth*: seq[cfloat] = @[]
    ## The width in pixels of the text with information about money
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
      trainCost(memberIndex = crewIndex, skillIndex = skillsIndexes[skillIndex])
    except:
      dialog = setError(message = "Can't count the training cost.")
      return
  timesCost = oneTrainCost * amount
  minCost = oneTrainCost
  if moneyIndex2 < 0:
    minCost = 0
    maxCost = 0
  else:
    maxCost = playerShip.cargo[moneyIndex2].amount

proc setSchool*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for school UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  moneyIndex2 = findItem(inventory = playerShip.cargo, protoIndex = moneyIndex)
  moneyText = @[]
  moneyWidth = @[]
  if moneyIndex == -1:
    moneyText.add(y = "You don't have any " & moneyName & " to pay for learning")
  else:
    moneyText.add(y = "You have ")
    moneyText.add(y = $playerShip.cargo[moneyIndex2].amount & " " & moneyName)
  for text in moneyText:
    try:
      moneyWidth.add(y = text.getTextWidth)
    except:
      dialog = setError(message = "Can't get the width of the money text.")
      return
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
  recruitsIndexes*: seq[Natural] = @[]
  baseIndex*: ExtendedBasesRange = 0

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
  pilotList*, engineerList*, gunnerList*: seq[string] = @["Nobody"]
  pilotIndex*, engineerIndex*: Natural = 0
  gunnersIndex*: seq[Natural] = @[]
  boardingParty*, defenders*: seq[bool] = @[]

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

type
  ItemsSortOrders* = enum
    nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc, priceAsc,
      priceDesc, profitAsc, profitDesc, weightAsc, weightDesc, ownedAsc,
      ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder*: ItemsSortOrders = none

var
  itemsSortOrder*: ItemsSortOrders = defaultItemsSortOrder
  itemsIndexes*: seq[int] = @[]
  baseCargo*: seq[BaseCargo] = @[]
  typesList*: seq[string] = @["All"]
  nameSearch*, location*, baseType*: string = ""
  cargoWidth*: array[2, cfloat] = [0.cfloat, 0]
  cargoText*: array[2, string] = ["Free cargo space is ", ""]
  typeIndex*: Natural = 0
  eventIndex*: int = -1

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
  moneyIndex2 = findItem(inventory = playerShip.cargo, protoIndex = moneyIndex)
  moneyText = @[]
  moneyWidth = @[]
  if moneyIndex == -1:
    moneyText.add(y = "You don't have " & moneyName & " to buy anything")
  else:
    moneyText.add(y = "You have ")
    moneyText.add(y = $playerShip.cargo[moneyIndex2].amount & " " & moneyName)
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
  var freeSpace = try:
      freeCargo(amount = 0)
    except:
      dialog = setError(message = "Can't get free space.")
      return
  if freeSpace < 0:
    freeSpace = 0
  cargoWidth[0] = try:
      cargoText[0].getTextWidth.cfloat
    except:
      dialog = setError(message = "Can't get the width of the cargo text.")
      0.0
  cargoText[1] = $freeSpace & " kg"
  cargoWidth[1] = try:
      cargoText[1].getTextWidth.cfloat
    except:
      dialog = setError(message = "Can't get the width of the cargo text.")
      0.0

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

