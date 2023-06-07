discard """
  exitcode: 0
"""

import std/tables
import ../../src/[game, goals, types]

if goalsList.len == 0:
  loadGoals("../bin/data/goals.dat")

currentGoal = goalsList[2]

var amount = currentGoal.amount
updateGoal(GoalTypes.destroy, "PIRATES", 1)
assert currentGoal.amount == (amount - 1), "Failed to update the current goal."
amount = currentGoal.amount
updateGoal(reputation, "PIRATES", 1)
assert currentGoal.amount == amount, "Failed to not update the current goal."
