discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, maps, items, ships, shipscrew, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

playerShip.skyX = 1
playerShip.skyY = 1
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100))
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
    1, 0, 0], order: gunner, loyalty: 100))
skyMap[1][1].baseIndex = 1

let oldMorale = playerShip.crew[0].morale[2]
let oldLevel = playerShip.crew[0].morale[1]
updateMorale(playerShip, 0, 1)
assert playerShip.crew[0].morale[2] == oldMorale + 1 or playerShip.crew[
    0].morale[1] == oldLevel + 1
updateMorale(playerShip, 0, -1)
assert playerShip.crew[0].morale[2] == oldMorale

giveOrders(playerShip, 0, rest)
assert playerShip.crew[0].order == talk

giveOrders(playerShip, 1, rest)
assert playerShip.crew[1].order == rest

giveOrders(playerShip, 0, rest, -1, false)
updateOrders(playerShip)
assert playerShip.crew[0].order == talk
