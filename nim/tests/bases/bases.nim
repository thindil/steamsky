discard """
  exitcode: 0
"""

import ../../src/[bases, careers, factions, game, items]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")

assert generateBaseName("POLEIS").len() > 0
