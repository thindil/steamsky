discard """
  exitcode: 0
"""

import ../../src/[game]

loadData("../bin/data/game.dat")

assert findSkillIndex(skillName = "Piloting") == 1
assert findSkillIndex(skillName = "sdfwerwerwe") == 0
