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

var currentGoal* = GoalData(multiplier: 1) ## The player's current goal
