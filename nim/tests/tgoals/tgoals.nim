discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateGoal.
Testing clearCurrentGoal.'''
"""

import std/tables
import ../../src/[game, goals, types]

echo "Loading the game data."
if goalsList.len == 0:
  loadGoals("../bin/data/goals.dat")

currentGoal = goalsList[2]

echo "Testing updateGoal."
var amount = currentGoal.amount
updateGoal(GoalTypes.destroy, "PIRATES", 1)
try:
  assert currentGoal.amount == (amount - 1)
except AssertionDefect:
  echo "Failed to update the current goal."
amount = currentGoal.amount
updateGoal(reputation, "PIRATES", 1)
try:
  assert currentGoal.amount == amount
except AssertionDefect:
  echo "Failed to not update the current goal."

echo "Testing clearCurrentGoal."
clearCurrentGoal()
try:
  assert currentGoal.index.len == 0
except AssertionDefect:
  echo "Failed to reset the player's current goal."
currentGoal = goalsList[1]
