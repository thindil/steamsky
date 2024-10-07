# Copyright 2023-2024 Bartek thindil Jasicki
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

import std/[strutils, tables, xmlparser, xmltree]
import contracts
import game, log, messages, statistics, types, utils

var currentGoal* = GoalData(multiplier: 1) ## The player's current goal

proc loadGoals*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the goals data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    fileName.len > 0
  body:
    let goalsXml = try:
        loadXml(path = fileName)
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
            parseEnum[DataAction](goalNode.attr(name = "action").toLowerAscii)
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
            debugType = everything)
        continue
      var goal: GoalData = if goalAction == DataAction.update:
          try:
            goalsList[goalIndex]
          except ValueError:
            GoalData(multiplier: 1)
        else:
          GoalData(multiplier: 1)
      goal.index = $goalIndex
      var attribute = goalNode.attr(name = "type")
      if attribute.len() > 0:
        goal.goalType = try:
            parseEnum[GoalTypes](attribute.toLowerAscii)
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
            debugType = everything)
      else:
        logMessage(message = "Goal updated: '" & $goalIndex & "'",
            debugType = everything)
      goalsList[goalIndex] = goal

proc updateGoal*(goalType: GoalTypes; targetIndex: string;
    amount: Positive = 1) {.sideEffect, raises: [], tags: [], contractual.} =
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

proc clearCurrentGoal*() {.sideEffect, raises: [], tags: [], contractual.} =
  ## Reset the player's current goal
  currentGoal = GoalData(index: "", goalType: random, amount: 0,
      targetIndex: "", multiplier: 1)

proc goalText*(index: int): string {.sideEffect, raises: [KeyError], tags: [],
    contractual.} =
  ## Get information about the selected goal. If index doesn't exist in the
  ## list of goals, get information about the current goal of the player.
  ##
  ## * index - the index of the goal which description will be get
  ##
  ## Returns the string with information about the selected goal
  let goal = (if index > 0 and goalsList.hasKey(index): goalsList[
      index] else: currentGoal)
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
  result = result & $goal.amount
  case goal.goalType
  of reputation, visit:
    result = result & " base"
  of GoalTypes.destroy:
    result = result & " ship"
  of discover:
    result = result & " field"
  of GoalTypes.craft:
    result = result & " item"
  of mission:
    result = result & " mission"
  of kill:
    result = result & " enem"
  of random:
    discard
  if goal.goalType notin {random, kill} and goal.amount > 1:
    result = result & "s"
  case goal.goalType
  of discover:
    result = result & " of map"
  of kill:
    if goal.amount > 1:
      result = result & "ies in melee combat"
    else:
      result = result & "y in melee combat"
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
      let faction = factionsList[factionIndex]
      case factionType
      of name:
        return faction.name
      of memberName:
        return faction.memberName
      of pluralMemberName:
        return faction.pluralMemberName

  if goal.targetIndex.len > 0:
    var insertPosition = result.len - 4
    if goal.amount > 1:
      insertPosition.dec
    case goal.goalType
    of reputation, visit:
      result.insert(getFactionName(goal.targetIndex, name) & " ", insertPosition)
    of GoalTypes.destroy:
      var added = false
      for index, ship in protoShipsList:
        let shipIndex = try: goal.targetIndex.parseInt except ValueError: -1
        if index == shipIndex:
          result = result & ": " & ship.name
          added = true
          break
      if not added:
        result.insert(getFactionName(goal.targetIndex, name) & " ", insertPosition)
    of GoalTypes.craft:
      if recipesList[goal.targetIndex].resultIndex > 0:
        result = result & ": " & itemsList[recipesList[
            goal.targetIndex].resultIndex].name
      else:
        result = result & ": " & goal.targetIndex
    of mission:
      try:
        let missionType = parseEnum[MissionsTypes](
            goal.targetIndex.toLowerAscii)
        case missionType
        of deliver:
          result = result & ": Deliver items to bases"
        of patrol:
          result = result & ": Patrol areas"
        of destroy:
          result = result & ": Destroy ships"
        of explore:
          result = result & ": Explore areas"
        of passenger:
          result = result & ": Transport passengers to bases"
      except ValueError:
        discard
    of kill:
      insertPosition = result.len - 21
      if goal.amount > 1:
        insertPosition = insertPosition - 2
      var stopPosition = insertPosition + 6
      if goal.amount > 1:
        result[insertPosition .. stopPosition] = getFactionName(
            goal.targetIndex, pluralMemberName)
      else:
        result[insertPosition .. stopPosition] = getFactionName(
            goal.targetIndex, memberName)
    of random, discover:
      discard

# Temporary code for interfacing with Ada

type
  AdaGoalData = object
    index: cstring
    goalType: cint
    amount: cint
    targetIndex: cstring
    multiplier: cint

proc loadAdaGoals(fileName: cstring): cstring {.sideEffect, raises: [], tags: [
    WriteIOEffect, ReadIOEffect, RootEffect], exportc, contractual.} =
  ## Temporary C binding
  try:
    loadGoals(fileName = $fileName)
    return "".cstring
  except DataLoadingError:
    return getCurrentExceptionMsg().cstring

proc getAdaGoal(index: cint; adaGoal: var AdaGoalData) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  adaGoal = AdaGoalData(index: "".cstring, goalType: -1, amount: -1,
      targetIndex: "".cstring, multiplier: 0)
  let goal = try:
      goalsList[index]
    except KeyError:
      return
  adaGoal.index = goal.index.cstring
  adaGoal.goalType = goal.goalType.ord.cint
  adaGoal.amount = goal.amount.cint
  adaGoal.targetIndex = goal.targetIndex.cstring
  adaGoal.multiplier = goal.multiplier.cint

proc updateAdaGoal(goalType: cint; targetIndex: cstring;
    amount: cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  updateGoal(goalType = goalType.GoalTypes, targetIndex = $targetIndex,
      amount = amount.Positive)

proc getAdaCurrentGoal(goal: AdaGoalData) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  currentGoal = GoalData(index: $goal.index, goalType: goal.goalType.GoalTypes,
      amount: goal.amount.Natural, targetIndex: $goal.targetIndex,
      multiplier: goal.multiplier.Positive)

proc setAdaCurrentGoal(goal: var AdaGoalData) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  goal = AdaGoalData(index: currentGoal.index.cstring,
      goalType: currentGoal.goalType.ord.cint, amount: currentGoal.amount.cint,
      targetIndex: currentGoal.targetIndex.cstring,
      multiplier: currentGoal.multiplier.cint)

proc clearAdaCurrentGoal() {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  clearCurrentGoal()

proc goalAdaText(index: cint): cstring {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  try:
    return goalText(index = index).cstring
  except KeyError:
    return ""

proc getAdaGoalsAmount(): cint {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return goalsList.len.cint

proc setAdaCurrentGoal2(index: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  for goal in goalsList.values:
    if goal.index == $index:
      currentGoal = goal
      break
