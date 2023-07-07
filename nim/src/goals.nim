# Copyright 2023 Bartek thindil Jasicki
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

import std/[strutils, tables, xmlparser, xmltree]
import game, log, messages, statistics, types, utils

var currentGoal* = GoalData(multiplier: 1) ## The player's current goal

proc loadGoals*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## Load the goals data from the file
  ##
  ## * fileName - the name of the file to load
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
    amount: Positive = 1) {.sideEffect, raises: [], tags: [].} =
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

proc clearCurrentGoal*() =
  currentGoal = GoalData(index: "", goalType: random, amount: 0,
      targetIndex: "", multiplier: 1)

# Temporary code for interfacing with Ada

type
  AdaGoalData = object
    index: cstring
    goalType: cint
    amount: cint
    targetIndex: cstring
    multiplier: cint

proc loadAdaGoals(fileName: cstring): cstring {.sideEffect, raises: [], tags: [
    WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  try:
    loadGoals(fileName = $fileName)
    return "".cstring
  except DataLoadingError:
    return getCurrentExceptionMsg().cstring

proc getAdaGoal(index: cint; adaGoal: var AdaGoalData) {.raises: [], tags: [], exportc.} =
  adaGoal = AdaGoalData(index: "".cstring, goalType: -1, amount: -1,
      targetIndex: "".cstring, multiplier: 0)
  if index > goalsList.len:
    return
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
    amount: cint) {.raises: [], tags: [], exportc.} =
  updateGoal(goalType = goalType.GoalTypes, targetIndex = $targetIndex,
      amount = amount.Positive)

proc getAdaCurrentGoal(goal: AdaGoalData) {.raises: [], tags: [], exportc.} =
  currentGoal = GoalData(index: $goal.index, goalType: goal.goalType.GoalTypes,
      amount: goal.amount.Natural, targetIndex: $goal.targetIndex,
      multiplier: goal.multiplier.Positive)

proc setAdaCurrentGoal(goal: var AdaGoalData) {.raises: [], tags: [], exportc.} =
  goal = AdaGoalData(index: currentGoal.index.cstring,
      goalType: currentGoal.goalType.ord.cint, amount: currentGoal.amount.cint,
      targetIndex: currentGoal.targetIndex.cstring,
      multiplier: currentGoal.multiplier.cint)

proc clearAdaCurrentGoal() {.raises: [], tags: [], exportc.} =
  clearCurrentGoal()
