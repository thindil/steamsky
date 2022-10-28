discard """
  exitcode: 0
"""

import ../../src/[careers, crew, factions, game, items]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")

assert generateMemberName('M', "POLEIS").len() > 0
