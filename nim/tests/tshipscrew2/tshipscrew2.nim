discard """
  exitcode: 0
  output: '''Loading the game data.
Testing deleteMember.
Testing death.'''
"""

import std/tables
import ../../src/[careers, factions, game, maps, items, shipscrew2, types]

echo "Loading the game data."
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
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
    1, 0, 0], order: gunner, loyalty: 100))
skyMap[1][1].baseIndex = 1

echo "Testing deleteMember."
var oldLength = playerShip.crew.len
deleteMember(1, playerShip)
try:
  assert playerShip.crew.len == (oldLength - 1)
except AssertionDefect:
  echo "Failed to remove a crew member from the player's ship."

echo "Testing death."
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
    1, 0, 0], order: gunner, loyalty: 100))
playerShip.cargo = @[]
oldLength = playerShip.crew.len
death(1, "Test death", playerShip)
try:
  assert playerShip.crew.len == oldLength - 1
except AssertionDefect:
  echo "Failed to kill a crew member on the player's ship."
try:
  assert playerShip.cargo.len == 1
except AssertionDefect:
  echo "Failed to update the player's ship's cargo with a body."
