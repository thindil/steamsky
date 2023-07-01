discard """
  exitcode: 0
  output: '''Loading the game data.
Testing findSkillIndex.'''
"""

import ../../src/[game]

echo "Loading the game data."
loadData("../bin/data/game.dat")

echo "Testing findSkillIndex."
assert findSkillIndex(skillName = "Piloting") == 1, "Failed to find the index of the skill."
assert findSkillIndex(skillName = "sdfwerwerwe") == 0, "Failed to not find the index of the non-existing skill."
