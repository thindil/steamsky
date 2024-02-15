import unittest2
include ../src/goals

suite "Unit tests for goals module":

  checkpoint "Loading the game data."
  loadGoals("../bin/data/goals.dat")

  currentGoal = goalsList[2]

  test "Testing updateGoal.":
    var amount = currentGoal.amount
    checkpoint "Updating the current goal"
    updateGoal(GoalTypes.destroy, "PIRATES", 1)
    check:
      currentGoal.amount == (amount - 1)
    checkpoint "Not updating the current goal"
    amount = currentGoal.amount
    updateGoal(reputation, "PIRATES", 1)
    check:
      currentGoal.amount == amount

  test "Testing clearCurrentGoal.":
    clearCurrentGoal()
    check:
      currentGoal.index.len == 0

  test "Testing goalText.":
    currentGoal = goalsList[1]
    check:
      goalText(1) == "Gain max reputation in 1 base"
