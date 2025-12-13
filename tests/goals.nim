import unittest2
include ../src/goals

suite "Unit tests for goals module":

  checkpoint "Loading the game data."
  loadGoals("bin/data/goals.dat".Path)

  currentGoal = goalsList[2]

  test "Updating the current goal":
    let amount = currentGoal.amount
    updateGoal(GoalTypes.destroy, "PIRATES", 1)
    check:
      currentGoal.amount == (amount - 1)

  test "Not updating the current goal":
    let amount = currentGoal.amount
    updateGoal(reputation, "PIRATES", 1)
    check:
      currentGoal.amount == amount

  test "Clearing the current goal.":
    clearCurrentGoal()
    check:
      currentGoal.index.len == 0

  test "Getting a goal's text.":
    currentGoal = goalsList[1]
    check:
      goalText(1) == "Gain max reputation in 1 base"
