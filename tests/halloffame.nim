import std/paths
import unittest2
include ../src/halloffame

suite "Unit tests for halloffame module":
  for entry in hallOfFameArray.mitems:
    entry = HallOfFameData(name: "", points: 0, deathReason: "")
  gameStats.points = 100
  saveDirectory = "tests/".Path

  test "Updating Hall of Fame.":
    updateHallOfFame("TestPlayer", "TestDeath")
    check:
      hallOfFameArray[1].name == "TestPlayer"
