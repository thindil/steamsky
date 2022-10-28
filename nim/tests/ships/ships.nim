discard """
  exitcode: 0
"""

import ../../src/[careers, factions, game, items, ships]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")

assert getCabinQuality(10) == "Empty room"

assert generateShipName("POLEIS").len() > 0
