discard """
  exitcode: 0
"""

import ../../src/[bases, careers, factions, game, items, types]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")

assert generateBaseName("POLEIS").len() > 0

skyBases[1].reputation = ReputationData(level: 1, experience: 1)
gainRep(1, 1)
assert skyBases[1].reputation.experience == 2
gainRep(1, -1)
assert skyBases[1].reputation.experience == 1
