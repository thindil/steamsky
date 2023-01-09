discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, items, ships, shipscrew, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural], homeBase: 1, faction: "POLEIS"))
let oldMorale = playerShip.crew[0].morale[2]
let oldLevel = playerShip.crew[0].morale[1]
updateMorale(playerShip, 0, 1)
assert playerShip.crew[0].morale[2] == oldMorale + 1 or playerShip.crew[0].morale[1] == oldLevel + 1
updateMorale(playerShip, 0, -1)
assert playerShip.crew[0].morale[2] == oldMorale
