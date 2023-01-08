discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, items, ships]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

assert getCabinQuality(10) == "Empty room"

assert generateShipName("POLEIS").len() > 0
