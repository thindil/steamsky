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
import game, log

type
  GoalTypes = enum
    ## Types of in-game goals
    random, reputation, destroy, discover, visit, craft, mission, kill

  GoalData = object
    ## Used to store information about the in-game goals
    index: string       ## The index of the goal prototype
    goalType: GoalTypes ## The type of the goal
    amount: Natural     ## The amount of targets needed for finishe the goal
    targetIndex: string ## The index of the target needed for finish the goal. If empty
                        ## means all targets of the selected type (bases, ships, etc.)
    multiplier: Positive ## The muliplier for points awarded for finishing the goal

var
  currentGoal* = GoalData(multiplier: 1) ## The player's current goal
  goalsList* = initTable[Positive, GoalData]() ## The list of available goals in the game

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

proc updateGoal*(goalType: GoalTypes; targetIndex: string;
    amount: Positive = 1) =
  if goalType != currentGoal.goalType:
    return
  if targetIndex.toLowerAscii != currentGoal.targetIndex.toLowerAscii and
      currentGoal.targetIndex.len > 0:
    return
  currentGoal.amount = (if amount > currentGoal.amount: 0 else: currentGoal.amount - amount)
