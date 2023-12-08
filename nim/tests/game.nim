import ../src/game
import unittest2

suite "Unit tests for game module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")

  test "Testing findSkillIndex.":
    checkpoint "Find an index of an exisiting skill"
    check:
      findSkillIndex(skillName = "Piloting") == 1
    checkpoint "Not find an index of a non-existing skill"
    check:
      findSkillIndex(skillName = "sdfwerwerwe") == 0
