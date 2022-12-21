discard """
  exitcode: 0
"""

import ../../src/[factions, careers, game, items]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")

assert getReputation("POLEIS", "POLEIS") == 0
assert getReputation("POLEIS", "PIRATES") == -10

assert isFriendly("POLEIS", "INDEPENDENT")
assert not isFriendly("POLEIS", "PIRATES")
