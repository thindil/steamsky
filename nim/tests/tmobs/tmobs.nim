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
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")

let newMob = generateMob(5, "POLEIS")
assert newMob.attributes[0].level == 2
assert newMob.orderTime == 15
