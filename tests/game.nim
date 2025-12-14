import unittest2
include ../src/game

suite "Unit tests for game module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)

  test "Find an index of an exisiting skill":
    check:
      findSkillIndex(skillName = "Piloting") == 1

  test "Not find an index of a non-existing skill":
    check:
      findSkillIndex(skillName = "sdfwerwerwe") == 0
