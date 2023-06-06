discard """
  exitcode: 0
"""

import ../../src/[game]

loadData("../bin/data/game.dat")

assert findSkillIndex(skillName = "Piloting") == 1, "Failed to find the index of the skill."
assert findSkillIndex(skillName = "sdfwerwerwe") == 0, "Failed to not find the index of the non-existing skill."
