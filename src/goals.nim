# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides code related to the player's in-game goals, like loading them
## from files, clearing, updating or getting their text.

import std/[logging, paths, strutils, tables, xmlparser, xmltree]
import contracts
import game, log, messages, statistics, types, utils

var currentGoal*: GoalData = GoalData(multiplier: 1) ## The player's current goal

proc loadGoals*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the goals data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    ($fileName).len > 0
  body:
    let goalsXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load goals data file. Reason: " &
            getCurrentExceptionMsg())
    for goalNode in goalsXml:
      if goalNode.kind != xnElement:
        continue
      let
        goalIndex: Natural = try:
            goalNode.attr(name = "index").parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't add goal '" & goalNode.attr(name = "index") & "', invalid index.")
        goalAction: DataAction = try:
            parseEnum[DataAction](s = goalNode.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if goalAction in [update, remove]:
        if goalIndex > goalsList.len():
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $goalAction & " goal '" & $goalIndex & "', there is no goal with that index.")
      elif goalIndex < goalsList.len():
        raise newException(exceptn = DataLoadingError,
            message = "Can't add goal '" & $goalIndex & "', there is an goal with that index.")
      if goalAction == DataAction.remove:
        {.warning[ProveInit]: off.}
        {.warning[UnsafeDefault]: off.}
        goalsList.del(key = goalIndex)
        {.warning[ProveInit]: on.}
        {.warning[UnsafeDefault]: on.}
        logMessage(message = "Goal removed: '" & $goalIndex & "'",
            messageLevel = lvlInfo)
        continue
      var goal: GoalData = if goalAction == DataAction.update:
          try:
            goalsList[goalIndex]
          except ValueError:
            GoalData(multiplier: 1)
        else:
          GoalData(multiplier: 1)
      goal.index = $goalIndex
      var attribute: string = goalNode.attr(name = "type")
      if attribute.len() > 0:
        goal.goalType = try:
            parseEnum[GoalTypes](s = attribute.toLowerAscii)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
              message = "Can't " & $goalAction & " goal '" & $goalIndex & "', invalid type of goal.")
      attribute = goalNode.attr(name = "amount")
      if attribute.len() > 0:
        goal.amount = try:
            attribute.parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $goalAction & " goal '" & $goalIndex & "', invalid value for amount.")
      attribute = goalNode.attr(name = "target")
      if attribute.len() > 0:
        goal.targetIndex = attribute
      attribute = goalNode.attr(name = "multiplier")
      if attribute.len() > 0:
        goal.multiplier = try:
            attribute.parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $goalAction & " goal '" & $goalIndex & "', invalid value for multiplier.")
      if goalAction == DataAction.add:
        logMessage(message = "Goal added: '" & $goalIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Goal updated: '" & $goalIndex & "'",
            messageLevel = lvlInfo)
      goalsList[goalIndex] = goal

proc updateGoal*(goalType: GoalTypes; targetIndex: string;
    amount: Positive = 1) {.raises: [], tags: [], contractual.} =
  ## Update the player's current goal. If the goal is finished, select randomly
  ## a new one
  ##
  ## * goalType    - the type of the goal which was finished
  ## * targetIndex - the target of the goal which was finished
  ## * amount      - the amount in the goal which was finished
  if goalType != currentGoal.goalType:
    return
  if targetIndex.toLowerAscii != currentGoal.targetIndex.toLowerAscii and
      currentGoal.targetIndex.len > 0:
    return
  currentGoal.amount = (if amount > currentGoal.amount: 0 else: currentGoal.amount - amount)
  if currentGoal.amount == 0:
    updateFinishedGoals(index = currentGoal.index)
    addMessage(message = "You finished your goal. New goal is set.",
        mType = otherMessage, color = blue)
    while currentGoal.amount == 0:
      try:
        currentGoal = goalsList[getRandom(min = 1, max = goalsList.len)]
      except KeyError:
        discard

proc clearCurrentGoal*() {.raises: [], tags: [], contractual.} =
  ## Reset the player's current goal
  currentGoal = GoalData(index: "", goalType: random, amount: 0,
      targetIndex: "", multiplier: 1)

proc goalText*(index: int): string {.raises: [KeyError], tags: [],
    contractual.} =
  ## Get information about the selected goal. If index doesn't exist in the
  ## list of goals, get information about the current goal of the player.
  ##
  ## * index - the index of the goal which description will be get
  ##
  ## Returns the string with information about the selected goal
  let goal: GoalData = (if index > 0 and goalsList.hasKey(
      key = index): goalsList[index] else: currentGoal)
  case goal.goalType
  of reputation:
    result = "Gain max reputation in "
  of GoalTypes.destroy:
    result = "Destroy "
  of discover:
    result = "Discover "
  of visit:
    result = "Visit "
  of GoalTypes.craft:
    result = "Craft "
  of mission:
    result = "Finish "
  of kill:
    result = "Kill "
  of random:
    discard
  result &= $goal.amount
  case goal.goalType
  of reputation, visit:
    result &= " base"
  of GoalTypes.destroy:
    result &= " ship"
  of discover:
    result &= " field"
  of GoalTypes.craft:
    result &= " item"
  of mission:
    result &= " mission"
  of kill:
    result &= " enem"
  of random:
    discard
  if goal.goalType notin {random, kill} and goal.amount > 1:
    result &= "s"
  case goal.goalType
  of discover:
    result &= " of map"
  of kill:
    if goal.amount > 1:
      result &= "ies in melee combat"
    else:
      result &= "y in melee combat"
  else:
    discard

  type FactionNameType = enum
    name, memberName, pluralMemberName

  proc getFactionName(factionIndex: string;
      factionType: FactionNameType): string {.raises: [KeyError], tags: [],
      contractual.} =
    ## Get the name of the faction or its member's names (singular or plural)
    ##
    ## * factionIndex - the index of the faction which a name will be get
    ## * factionType  - the type of the name to get
    ##
    ## Returns a name related to the faction, depends on factionType argument.
    require:
      factionsList.hasKey(key = factionIndex)
    body:
      let faction: FactionData = factionsList[factionIndex]
      case factionType
      of name:
        return faction.name
      of memberName:
        return faction.memberName
      of pluralMemberName:
        return faction.pluralMemberName

  if goal.targetIndex.len > 0:
    var insertPosition: int = result.len - 4
    if goal.amount > 1:
      insertPosition.dec
    case goal.goalType
    of reputation, visit:
      result.insert(item = getFactionName(factionIndex = goal.targetIndex,
          factionType = name) & " ", i = insertPosition)
    of GoalTypes.destroy:
      var added: bool = false
      for index, ship in protoShipsList:
        let shipIndex: int = try: goal.targetIndex.parseInt except ValueError: -1
        if index == shipIndex:
          result = result & ": " & ship.name
          added = true
          break
      if not added:
        result.insert(item = getFactionName(factionIndex = goal.targetIndex,
            factionType = name) & " ", i = insertPosition)
    of GoalTypes.craft:
      if recipesList[goal.targetIndex].resultIndex > 0:
        result &= ": " & itemsList[recipesList[
            goal.targetIndex].resultIndex].name
      else:
        result &= ": " & goal.targetIndex
    of mission:
      try:
        let missionType: MissionsTypes = parseEnum[MissionsTypes](
            s = goal.targetIndex.toLowerAscii)
        case missionType
        of deliver:
          result &= ": Deliver items to bases"
        of patrol:
          result &= ": Patrol areas"
        of destroy:
          result &= ": Destroy ships"
        of explore:
          result &= ": Explore areas"
        of passenger:
          result &= ": Transport passengers to bases"
      except ValueError:
        discard
    of kill:
      insertPosition = result.len - 21
      if goal.amount > 1:
        insertPosition -= 2
      var stopPosition: int = insertPosition + 6
      if goal.amount > 1:
        result[insertPosition..stopPosition] = getFactionName(
            factionIndex = goal.targetIndex, factionType = pluralMemberName)
      else:
        result[insertPosition..stopPosition] = getFactionName(
            factionIndex = goal.targetIndex, factionType = memberName)
    of random, discover:
      discard
