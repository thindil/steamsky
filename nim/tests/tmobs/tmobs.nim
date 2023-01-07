discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, items, mobs, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

let itemIndex = getRandomItem(weaponsList, weapon, 20, 20, "POLEIS")
assert itemIndex > 0 and itemsList.hasKey(itemIndex)
